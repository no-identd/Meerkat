package org.meerkat.graph.neo4j

import org.meerkat.Syntax._
import org.meerkat.graph.neo4j.Neo4jInput.Entity
import org.meerkat.parsers.Parsers._
import org.meerkat.parsers._
import org.neo4j.graphdb.GraphDatabaseService

class Neo4jGraphTest3 extends Neo4jGraphStatisticsTest("3") {
  val S: Nonterminal[Entity, Entity] = syn(
    outE((_: Entity).label() == "a") ~ outE((_: Entity).label() == "b") ~ S
      | ε
  )

  override def fillDb(db: GraphDatabaseService): Unit = {
    val n1 = db.createNode
    val n2 = db.createNode
    n1.createRelationshipTo(n2, () => "a")
    n2.createRelationshipTo(n1, () => "b")
  }

  override def createParser: Nonterminal[Entity, Entity] = S

  override def expectedSppfStatistics: SPPFStatistics =
    SPPFStatistics(1, 2, 3, 4, 1)
}
