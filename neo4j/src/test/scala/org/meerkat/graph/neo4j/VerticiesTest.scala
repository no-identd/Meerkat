package org.meerkat.graph.neo4j

import org.meerkat.parsers.Parsers.Nonterminal
import org.scalatest.{FunSuite, Matchers}
import org.meerkat.Syntax._
import org.meerkat.input.LinearInput._
import org.meerkat.parsers.Parsers._
import org.meerkat.parsers._
import org.neo4j.graphdb.{GraphDatabaseService, Label}

class VerticiesTest extends Neo4jGraphTest("verticiesTest") with Matchers {

  override def fillDb(db: GraphDatabaseService): Unit = {
    val n1 = db.createNode(Label.label("1"))
    val n2 = db.createNode(Label.label("2"))
    n1.createRelationshipTo(n2, () => "+")
  }

  override def createParser: AbstractCPSParsers.AbstractSymbol[String, _, _] = {
    val num = V((_: String) forall Character.isDigit)
    syn(num ~ "+" ~ num)
  }

  override def doTest(parser: AbstractCPSParsers.AbstractSymbol[String, _, _],
                      graph: Neo4jInput,
                      db: GraphDatabaseService): Unit = {
    getSPPFs(parser, graph) shouldBe 'Right
  }
}
