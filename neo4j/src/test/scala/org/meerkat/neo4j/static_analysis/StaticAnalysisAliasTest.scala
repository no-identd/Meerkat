package org.meerkat.neo4j.static_analysis

import org.meerkat.Syntax.syn
import org.meerkat.graph._
import org.meerkat.parsers.Parsers._
import org.meerkat.parsers._
import org.meerkat.neo4j.{EmbeddedNeo4jGraph, Neo4jGraphTest}
import org.meerkat.parsers.AbstractCPSParsers
import org.neo4j.graphdb.{GraphDatabaseService, Label, Node}

import scala.collection.JavaConverters._

abstract class StaticAnalysisAliasTest extends Neo4jGraphTest("basicStaticAnalysis") {

  private val grammar = new AnyRef {
    val ALL: Nonterminal = syn(M | V)
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


  override def doTest(parser: AbstractCPSParsers.AbstractSymbol[_, _],
                      graph: EmbeddedNeo4jGraph,
                      db: GraphDatabaseService): Unit = {
    def getName(i: Int) =
      db.getNodeById(i).getLabels.asScala.mkString

    val pairs =
      (for {
        root <- parseGraphFromAllPositions(parser, graph, Some(List("M", "V")))
        nontermName = root.name.toString
        Seq(leftName, rightName) = Seq(getName(root.leftExtent), getName(root.rightExtent)).sorted
        if leftName != rightName
      } yield (nontermName, leftName, rightName)).distinct
    println(s"Count of aliases is ${pairs.size}")
    for (p <- pairs.sortBy(_._1))
      println(p)
  }

  override def createParser: AbstractCPSParsers.AbstractSymbol[_, _] = grammar.ALL
}
