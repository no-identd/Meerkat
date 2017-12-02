package org.meerkat.neo4j.static_analysis

import org.meerkat.Syntax._
import org.meerkat.graph._
import org.meerkat.neo4j.{EmbeddedNeo4jGraph, Neo4jGraphTest}
import org.meerkat.parsers.AbstractCPSParsers
import org.meerkat.parsers.Parsers._
import org.meerkat.parsers._
import org.neo4j.graphdb.{GraphDatabaseService, Label, Node}

import scala.collection.JavaConverters._

class BasicStaticAnalysisTest extends Neo4jGraphTest("basicStaticAnalysis") {
  private val grammar = new AnyRef {
    val M: Nonterminal = syn(DV ~~ "d")
    val V: Nonterminal = syn(MAM ~~ AMs)

    private val DV: Nonterminal = syn("nd" ~~ V)
    private val MAM: Nonterminal = syn(MAs ~~ Mq)
    private val Mq: Nonterminal = syn(epsilon | M)
    private val MAs: Nonterminal = syn(epsilon | MAs ~~ MA)
    private val MA: Nonterminal = syn(Mq ~~ "na")
    private val AMs: Nonterminal = syn(epsilon | AMs ~~ AM)
    private val AM: Nonterminal = syn("a" ~~ Mq)
  }

  override def fillDb(db: GraphDatabaseService): Unit = {
    val a = db.createNode(Label.label("a"))
    val b = db.createNode(Label.label("b"))
    val c = db.createNode(Label.label("c"))
    val d = db.createNode(Label.label("d"))
    val e = db.createNode(Label.label("e"))
    val `&b` = db.createNode(Label.label("&b"))
    val `&c` = db.createNode(Label.label("&c"))
    val `&d` = db.createNode(Label.label("&d"))
    val `*c` = db.createNode(Label.label("*c"))
    val `*d` = db.createNode(Label.label("*d"))

    def createEdgeWithReversed(f: Node, t: Node, l: String) = {
      f.createRelationshipTo(t, () => l)
      t.createRelationshipTo(f, () => "n" + l)
    }

    createEdgeWithReversed(a,    b,    "a")
    createEdgeWithReversed(`&b`, b,    "d")
    createEdgeWithReversed(`&b`, c,    "a")
    createEdgeWithReversed(`&c`, c,    "d")
    createEdgeWithReversed(c,    `*c`, "d")
    createEdgeWithReversed(e,    `*c`, "a")
    createEdgeWithReversed(`*c`, `*d`, "a")
    createEdgeWithReversed(d,    `*d`, "d")
    createEdgeWithReversed(`&d`,  d,   "d")
  }

  override def doTest(parser: AbstractCPSParsers.AbstractSymbol[_, _],
                      graph: EmbeddedNeo4jGraph,
                      db: GraphDatabaseService): Unit = {
    def getName(i: Int) =
      db.getNodeById(i).getLabels.asScala.mkString
    val pairs =
      (for {
        root <- parseGraphFromAllPositions(parser, graph)
        Seq(leftName, rightName) = Seq(getName(root.leftExtent), getName(root.rightExtent)).sorted
        if leftName != rightName
      } yield (leftName, rightName)).toSet

    for (p <- pairs)
      println(p)
  }
  override def createParser: AbstractCPSParsers.AbstractSymbol[_, _] = grammar.V
}
