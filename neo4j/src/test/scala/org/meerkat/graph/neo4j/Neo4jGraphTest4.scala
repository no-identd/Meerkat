package org.meerkat.graph.neo4j

import org.meerkat.Syntax._
import org.meerkat.parsers.Parsers._
import org.meerkat.parsers._
import org.neo4j.graphdb.GraphDatabaseService

class Neo4jGraphTest4 extends Neo4jGraphStatisticsTest("4") {
  val S: Nonterminal[String, String] = syn(
    terminal("(") ~ S ~ terminal(")") ~ S | epsilon
  )

  override def fillDb(db: GraphDatabaseService): Unit = {
    val n = List.fill(4)(db.createNode)
    n(0).createRelationshipTo(n(1), () => "(")
    n(1).createRelationshipTo(n(2), () => ")")
    n(2).createRelationshipTo(n(0), () => "(")
    n(0).createRelationshipTo(n(3), () => ")")
    n(3).createRelationshipTo(n(0), () => ")")
  }

  override def createParser: Nonterminal[String, String] = S

  override def expectedSppfStatistics: SPPFStatistics =
    SPPFStatistics(8, 13, 9, 24, 3)
}
