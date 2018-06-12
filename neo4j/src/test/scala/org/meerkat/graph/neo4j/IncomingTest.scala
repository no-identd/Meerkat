package org.meerkat.graph.neo4j

import org.meerkat.Syntax._
import org.meerkat.graph.neo4j.Neo4jInput._
import org.meerkat.parsers.Parsers._
import org.meerkat.parsers._
import org.meerkat.sppf.NonPackedNode
import org.neo4j.graphdb.{Direction, GraphDatabaseService, Label}
import org.scalatest.Matchers
import org.meerkat.graph.neo4j.Neo4jParsers._

import collection.JavaConverters._

class IncomingTest extends Neo4jGraphTest("incomingTest") with Matchers {

  override def fillDb(db: GraphDatabaseService): Unit = {
    val n1 = db.createNode(Label.label("1"))
    val n2 = db.createNode(Label.label("2"))
    n1.createRelationshipTo(n2, () => "+")

  }

  override def createParser
    : AbstractCPSParsers.AbstractSymbol[Entity, Entity, NonPackedNode, _] = {
    syn(LV("2") ~ inE((e: Entity) => e.label() == "+") ~ LV("1"))
  }

  override def doTest(parser: AbstractCPSParsers.AbstractSymbol[Entity,
                                                                Entity,
                                                                NonPackedNode,
                                                                _],
                      graph: Neo4jInput,
                      db: GraphDatabaseService): Unit = {
    executeQuery(parser, graph) should not be empty
  }
}
