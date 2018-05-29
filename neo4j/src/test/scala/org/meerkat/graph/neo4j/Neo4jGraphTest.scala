package org.meerkat.graph.neo4j

import org.meerkat.parsers.{AbstractCPSParsers, _}
import org.neo4j.graphdb.GraphDatabaseService
import org.neo4j.test.TestGraphDatabaseFactory
import org.meerkat.graph.neo4j.Neo4jInput._
import org.meerkat.sppf.NonPackedNode
import org.scalatest.FunSuite
import org.scalatest.Matchers._
import org.scalatest.OptionValues._

abstract class Neo4jGraphTest(name: String) extends FunSuite {
  def fillDb(db: GraphDatabaseService): Unit

  def createParser: AbstractCPSParsers.AbstractSymbol[Entity, Entity, NonPackedNode, _]

  def doTest(parser: AbstractCPSParsers.AbstractSymbol[Entity, Entity, NonPackedNode, _], graph: Neo4jInput, db: GraphDatabaseService)

  test(s"Neo4jGraphTest_$name") {
    val db = new TestGraphDatabaseFactory().newImpermanentDatabase
    val tx = db.beginTx()
    fillDb(db)
    val parser = createParser
    val graph  = new Neo4jInput(db)
    doTest(parser, graph, db)
    tx.success()
    db.shutdown()
  }
}

abstract class Neo4jGraphStatisticsTest(name: String) extends Neo4jGraphTest(name) {
  override def doTest(parser: AbstractCPSParsers.AbstractSymbol[Entity, Entity, NonPackedNode, _],
                      graph: Neo4jInput,
                      db: GraphDatabaseService): Unit =
    parseGraphAndGetSppfStatistics(parser, graph).value shouldBe expectedSppfStatistics

  def expectedSppfStatistics: SPPFStatistics
}
