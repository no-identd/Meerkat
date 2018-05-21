package org.meerkat.graph.neo4j

import org.meerkat.Syntax._
import org.meerkat.graph.neo4j.Neo4jInput._
import org.meerkat.parsers.Parsers._
import org.meerkat.parsers._
import org.neo4j.graphdb.{GraphDatabaseService, Label}
import org.scalatest.Matchers

class PropertiesTest extends Neo4jGraphTest("verticiesTest") with Matchers {

  override def fillDb(db: GraphDatabaseService): Unit = {
    val n1 = db.createNode
    n1.setProperty("nya", true)
    val n2 = db.createNode
    n2.setProperty("foo", 42)
    n1.createRelationshipTo(n2, () => "a").setProperty("cake", "lie")
  }

  override def createParser: AbstractCPSParsers.AbstractSymbol[Entity, Entity,  _, _] = {
    syn(V((_: Entity).nya == true) ~ E((_: Entity).cake == "lie") ~ V((_: Entity).foo == 42))
  }

  override def doTest(parser: AbstractCPSParsers.AbstractSymbol[Entity, Entity,  _, _],
                      graph: Neo4jInput,
                      db: GraphDatabaseService): Unit = {
    getSPPFs(parser, graph) shouldBe 'Right
  }
}
