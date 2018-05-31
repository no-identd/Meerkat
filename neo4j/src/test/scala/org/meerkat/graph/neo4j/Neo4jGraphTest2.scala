package org.meerkat.graph.neo4j

import org.meerkat.Syntax._
import org.meerkat.graph.neo4j.Neo4jInput.Entity
import org.meerkat.parsers.Parsers.{outE, _}
import org.meerkat.parsers._
import org.neo4j.graphdb.GraphDatabaseService

class Neo4jGraphTest2 extends Neo4jGraphStatisticsTest("2") {
  override def fillDb(db: GraphDatabaseService): Unit = {
    val n1 = db.createNode
    val n2 = db.createNode
    val n3 = db.createNode
    n1.createRelationshipTo(n2, () => "a")
    n2.createRelationshipTo(n3, () => "b")
  }

  override def createParser: Nonterminal[Entity, Entity] & NoValue =
    syn(outE((_: Entity).label() == "a") ~ outE((_: Entity).label() == "b"))

  override def expectedSppfStatistics: SPPFStatistics =
    SPPFStatistics(1, 1, 2, 2, 0)
}
