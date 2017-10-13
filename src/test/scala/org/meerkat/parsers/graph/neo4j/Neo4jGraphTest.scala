package org.meerkat.parsers.graph.neo4j

import java.io.File

import org.meerkat.parsers.graph.parseGraphAndGetSppfStatistics
import org.meerkat.parsers.{AbstractCPSParsers, ParseGraphSuccess, SPPFStatistics, parseGraph}
import org.meerkat.util.EmbeddedNeo4hGraph
import org.meerkat.util.visualization.visualize
import org.neo4j.graphdb.{GraphDatabaseService, Transaction}
import org.neo4j.graphdb.factory.GraphDatabaseFactory
import org.scalatest.{BeforeAndAfter, FunSuite}
import org.scalatest.Matchers._
import org.scalatest.OptionValues._
import org.neo4j.test.TestGraphDatabaseFactory


abstract class Neo4jGraphTest(name: String) extends FunSuite {
  def fillDb(db: GraphDatabaseService): Unit

  def createParser: AbstractCPSParsers.AbstractSymbol[_, _]

  def expectedSppfStatistics: SPPFStatistics

  test(name) {
    val db = new TestGraphDatabaseFactory().newImpermanentDatabase
    val tx = db.beginTx()
    fillDb(db)
    val parser = createParser
    val graph = new EmbeddedNeo4hGraph(db)
    parseGraphAndGetSppfStatistics(parser, graph).value shouldBe expectedSppfStatistics
    tx.success()
    db.shutdown()
  }
}
