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

  val common = new AnyRef {
    val actor = V((_: Entity).hasLabel("Actor"))
    def movie = V((_: Entity).hasLabel("Movie"))
    def movie(title: String) =
      V((e: Entity) => e.hasLabel("Movie") && e.title == title)
    val actsIn = E((_: Entity).label() == "ACTS_IN")
  }

  def query1()(implicit input: Neo4jInput): Unit = {
    import common._
    val query = syn((syn(LV("Actor") ^ (_.getProperty[String]("name")))
        ~ LE("ACTS_IN") ~ (LV("Movie") :: V((_:Entity).title == "Forrest Gump"))) &&)

    executeQuery(query, input).foreach(println)
  }

  def query2()(implicit input: Neo4jInput): Unit = {
    import common._
    val query = syn((syn(LV("Actor") ^^) ~ LE("ACTS_IN") ~ LV("Movie")) &
      ((a: Entity) => (a.getProperty[String]("name"), a.id.asInstanceOf[String].toInt)))

    executeQuery(query, input)
      .groupBy({case (a, i) => i})
      .toIndexedSeq
      .map({case (i, ms) => (ms.head._1, ms.length)})
      .sortBy({case (a, mc) => -mc})
      .take(10)
      .foreach({case (a, mc) => println(a, mc)})
  }

  def query3()(implicit input: Neo4jInput): Unit = {
    val dirs = syn((syn(LV("Actor", "Director") ^^) ~ LE("Directed") ~ LV("Movie"))
                    & (d  => d.id.asInstanceOf[String].toInt))

    val directors = executeQuery(dirs, input)
      .groupBy(i => i)
      .map({case (i, ms) => (i, ms.length)})
      .filter({case (_, ms) => ms >= 2})

    val actor_prof_director = syn(LV("Actor", "Director") :: V((e: Entity) => directors.contains(e.id.asInstanceOf[String].toInt)) ^^)

    val acts = syn((actor_prof_director ~ LE("ACTS_IN") ~ LV("Movie")) &
      (a => (a.getProperty[String]("name"), a.id.asInstanceOf[String].toInt)))

    executeQuery(acts, input)
      .groupBy({case (a, i) => i})
      .toStream
      .map({case (i, ms) => (i, ms.head._1, ms.length)})
      .filter({case (i, a, mc) => mc >= 10})
      .foreach({case (i, a, mc) => println((a, mc, directors(i)))})
  }

  def query4()(implicit input: Neo4jInput): Unit = {
    val adilfulara = syn(LV("User") :: V((_: Entity).login == "adilfulara"))

    val query = syn((adilfulara ~ LE("FRIEND") ~ syn(LV("Person") ^^) ~
                                 syn(LE("RATED") ^^) ~ syn(LV("Movie") ^^)) &
      {case p ~ r ~ m => (p.getProperty[String]("name"), m.title, r.stars.asInstanceOf[Int],
                          if (r.hasProperty("comment")) r.comment else "")})

    executeQuery(query, input)
      .filter({case (_, _, s, _) => s > 3})
      .foreach(println)
  }



  start()
}
