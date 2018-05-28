package org.meerkat.graph.neo4j

import java.io.File

import org.meerkat.graph.neo4j.Neo4jInput.Entity
import org.neo4j.test.TestGraphDatabaseFactory
import org.meerkat.Syntax._
import org.meerkat.graph.neo4j.Neo4jInput._
import org.meerkat.parsers.Parsers._
import org.meerkat.parsers._
import org.meerkat.sppf.NonPackedNode
import org.neo4j.graphdb.{GraphDatabaseService, Label}
import org.neo4j.graphdb.factory.GraphDatabaseFactory
import org.neo4j.kernel.internal.EmbeddedGraphDatabase
import org.scalameter.api._
import org.scalameter.{Key, Quantity, Warmer, config}

object MoviesDatasetBenchmark extends App {
  def start(): Unit = {
    val db = new GraphDatabaseFactory()
      .newEmbeddedDatabaseBuilder(new File("../graph.db"))
      .setConfig("dbms.allow_format_migration", "true")
      .newGraphDatabase()

    val tx = db.beginTx()

    implicit val input = new Neo4jInput(db)

    println(measureExecutionTime(query1))

    tx.success()

    db.shutdown()
  }

  private def measureExecutionTime(query: () => Unit)(implicit input: Neo4jInput) = {
    val time = config(
      Key.exec.benchRuns -> 5,
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
    val actor = syn(V((e: Entity) => e.hasLabel(Label.label("Actor"))) ^ (a => a.name))
    val actsIn = syn(E((e: Entity) => e.value() == "ACTS_IN"))
    val query = syn((actor ~ actsIn ~ V((e: Entity) => e.hasLabel(Label.label("Movie")) && e.title == "Forrest Gump")) &&)

    executeQuery(query, input).foreach(println)
  }

  start()
}
