package org.meerkat.parsers.graph.neo4j

import org.neo4j.graphdb.{GraphDatabaseService, Label}
import org.meerkat.Syntax._
import org.meerkat.parsers.Parsers._
import org.meerkat.parsers._

class Neo4jGraphTest1 extends Neo4jGraphTest("1") {
  override def fillDb(db: GraphDatabaseService): Unit = {
    val n1 = db.createNode
    val n2 = db.createNode
    n1.createRelationshipTo(n2, () => "a")
  }

  override def createParser: Nonterminal&NoValue = syn("a")

  override def expectedSppfStatistics: SPPFStatistics =
    SPPFStatistics(1, 0, 1, 1, 0)
}
