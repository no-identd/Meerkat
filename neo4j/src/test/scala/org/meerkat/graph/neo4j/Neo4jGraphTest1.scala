package org.meerkat.graph.neo4j

import org.meerkat.Syntax._
import org.meerkat.graph.neo4j.Neo4jInput.Entity
import org.meerkat.parsers.Parsers._
import org.meerkat.parsers._
import org.neo4j.graphdb.{GraphDatabaseService, Label}

class Neo4jGraphTest1 extends Neo4jGraphStatisticsTest("1") {
  override def fillDb(db: GraphDatabaseService): Unit = {
    val n1 = db.createNode
    val n2 = db.createNode
    n1.createRelationshipTo(n2, () => "a")
  }

  override def createParser: Nonterminal[Entity, Entity] & NoValue =
    syn(E((_: Entity).value() == "a"))

  override def expectedSppfStatistics: SPPFStatistics =
    SPPFStatistics(1, 0, 1, 1, 0)
}
