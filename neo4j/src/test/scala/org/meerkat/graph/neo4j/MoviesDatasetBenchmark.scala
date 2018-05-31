package org.meerkat.graph.neo4j

import java.io.{BufferedWriter, File, FileWriter}

import org.meerkat.graph.neo4j.Neo4jInput.Entity
import org.neo4j.test.TestGraphDatabaseFactory
import org.meerkat.Syntax._
import org.meerkat.graph.neo4j.Neo4jInput._
import org.meerkat.graph.neo4j.Neo4jParsers._
import org.meerkat.parsers.Parsers._
import org.meerkat.parsers._
import org.meerkat.sppf.NonPackedNode
import org.neo4j.graphdb.{GraphDatabaseService, Label}
import org.neo4j.graphdb.factory.GraphDatabaseFactory
import org.neo4j.kernel.internal.EmbeddedGraphDatabase
import org.scalameter.api._
import org.scalameter.{Key, Quantity, Warmer, config}

object MoviesDatasetBenchmark extends App {
  implicit private def toLabel(name: String) = Label.label(name)

  def start(): Unit = {
    val db = new GraphDatabaseFactory()
      .newEmbeddedDatabaseBuilder(new File(args(0)))
      .setConfig("dbms.allow_format_migration", "true")
      .newGraphDatabase()

    val tx = db.beginTx()
    implicit val input = new Neo4jInput(db)

    val log = new BufferedWriter(new FileWriter(new File("./MoviesDatasetBenchmark.log")))

    log.write("query 4 (Mutual Friend recommendations): " + measureExecutionTime(query4) + "\n")
    log.write("query 3 (Directed >= 2, Acted in >= 10): " + measureExecutionTime(query3) + "\n")
    log.write("query 2 (Find the most profilic actors): " + measureExecutionTime(query2) + "\n")
    log.write("query 1 (Actors who played in 'Forrest Gump'): " + measureExecutionTime(query1) + "\n")

    log.close()

    tx.success()
    db.shutdown()
  }

  private def measureExecutionTime(query: () => Unit)(implicit input: Neo4jInput) = {
    val time = config(
      Key.exec.benchRuns -> 3,
      Key.verbose -> true
    ) withWarmer {
      new Warmer.Default
    } withMeasurer {
      new Measurer.IgnoringGC
    } measure {
      query()
    }
    time
  }

  def query1()(implicit input: Neo4jInput): Unit = {
    val actor = syn(V((e: Entity) => e.hasLabel("Actor")) ^ (a => a.getProperty[String]("name")))
    val actsIn = syn(E((e: Entity) => e.value() == "ACTS_IN"))
    val query = syn((actor ~ actsIn ~ V((e: Entity) => e.hasLabel("Movie") && e.title == "Forrest Gump")) &&)

    executeQuery(query, input).foreach(println)
  }

  def query2()(implicit input: Neo4jInput): Unit = {
    val actor = syn(V((e: Entity) => e.hasLabel(Label.label("Actor"))) ^^)
    val actsIn = syn(E((e: Entity) => e.value() == "ACTS_IN"))
    val movie = syn(V((e: Entity) => e.hasLabel(Label.label("Movie"))))
    val query = syn((actor ~ actsIn ~ movie) & (a => (a.getProperty[String]("name"), a.id.asInstanceOf[String].toInt)))

    executeQuery(query, input)
      .groupBy({case (a, i) => i})
      .toIndexedSeq
      .map({case (i, ms) => (ms.head._1, ms.length)})
      .sortBy({case (a, mc) => -mc})
      .take(10)
      .foreach({case (a, mc) => println(a, mc)})
  }

  def query3()(implicit input: Neo4jInput): Unit = {
    val actor_director = syn(V((e: Entity) => e.hasLabel("Director") && e.hasLabel("Actor")) ^^)
    val directed = syn(E((e: Entity) => e.value() == "DIRECTED"))
    val movie = syn(V((e: Entity) => e.hasLabel(Label.label("Movie"))))

    val dirs = syn((actor_director ~ directed ~ movie) & (d  => d.id.asInstanceOf[String].toInt))

    val directors = executeQuery(dirs, input)
      .groupBy(i => i)
      .map({case (i, ms) => (i, ms.length)})
      .filter({case (_, ms) => ms >= 2})

    val actor_prof_director = syn(V((e: Entity) => e.hasLabel("Director") && e.hasLabel("Actor") && directors.contains(e.id.asInstanceOf[String].toInt)) ^^)
    val actsIn = syn(E((e: Entity) => e.value() == "ACTS_IN"))

    val acts = syn((actor_prof_director ~ actsIn ~ movie) & (a => (a.getProperty[String]("name"), a.id.asInstanceOf[String].toInt)))

    executeQuery(acts, input)
      .groupBy({case (a, i) => i})
      .toStream
      .map({case (i, ms) => (i, ms.head._1, ms.length)})
      .filter({case (i, a, mc) => mc >= 10})
      .foreach({case (i, a, mc) => println((a, mc, directors(i)))})
  }

  def query4()(implicit input: Neo4jInput): Unit = {
    val adilfulara = syn(LV("User") :: V((_: Entity).login == "adilfulara") ^^)

    val query = syn((adilfulara ~ LE("FRIEND") ~ (LV("Person") ^^) ~
                                 (LE("RATED") ^^) ~ (LV("Movie") ^^)) &
      {case p ~ r ~ m => (p.getProperty[String]("name"), m.title, r.stars.asInstanceOf[Int],
                          if (r.hasProperty("comment")) r.comment else "")})

    executeQuery(query, input)
      .filter({case (_, _, s, _) => s > 3})
      .foreach(println)
  }



  start()
}
