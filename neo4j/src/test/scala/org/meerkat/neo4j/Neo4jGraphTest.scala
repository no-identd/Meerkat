package org.meerkat.neo4j

import org.meerkat.parsers._
import org.neo4j.graphdb.GraphDatabaseService
import org.neo4j.test.TestGraphDatabaseFactory
import org.scalatest.FunSuite
import org.scalatest.Matchers._
import org.scalatest.OptionValues._


abstract class Neo4jGraphTest(name: String) extends FunSuite {
  def fillDb(db: GraphDatabaseService): Unit

  def createParser: AbstractCPSParsers.AbstractSymbol[_, _]

  def expectedSppfStatistics: SPPFStatistics

  test(s"Neo4jGraphTest_$name") {
    val db = new TestGraphDatabaseFactory().newImpermanentDatabase
    val tx = db.beginTx()
    fillDb(db)
    val parser = createParser
    val graph = new EmbeddedNeo4jGraph(db)
    parseGraphAndGetSppfStatistics(parser, graph).value shouldBe expectedSppfStatistics
    tx.success()
    db.shutdown()
  }
}
