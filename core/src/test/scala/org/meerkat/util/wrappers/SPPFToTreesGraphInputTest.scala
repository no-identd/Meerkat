package org.meerkat.util.wrappers

import org.meerkat.Syntax._
import org.meerkat.input.GraphxInput
import org.meerkat.parsers.Parsers.{Nonterminal, _}
import org.meerkat.parsers._
import org.meerkat.parsers.examples.Num
import org.meerkat.tree._
import org.meerkat.util.wrappers.TestUtils._
import org.scalatest.{FunSuite, Matchers}

import scalax.collection.Graph
import scalax.collection.edge.Implicits._
import scalax.collection.edge.LkDiEdge

class SPPFToTreesGraphInputTest extends FunSuite with Matchers {
  test("NonAmbiguousGrammarTwoPathsTestCorrectness") {
    var S: Nonterminal[String] = null
    S = syn("x" ~ "+" ~ "x" | "x" ~ "-" ~ "x")

    val graph = Graph(
      (0 ~+#> 1)("x"),
      (1 ~+#> 2)("+"),
      (1 ~+#> 3)("-"),
      (2 ~+#> 4)("x"),
      (3 ~+#> 4)("x"))

    val trees = getTrees(graph, S).toList

    trees.size shouldBe 2

    val rule1 = Rule(S.symbol, Sequence(TerminalSymbol("x"), TerminalSymbol("+"), TerminalSymbol("x")))
    val rule2 = Rule(S.symbol, Sequence(TerminalSymbol("x"), TerminalSymbol("-"), TerminalSymbol("x")))

    compareTreesIgnoringExtents(trees(0),
      RuleNode(rule1, Seq(terminalNode("x"), terminalNode("+"), terminalNode("x")))
    ) shouldBe true

    compareTreesIgnoringExtents(trees(1),
      RuleNode(rule2, Seq(terminalNode("x"), terminalNode("-"), terminalNode("x")))
    ) shouldBe true
  }

  test("NonAmbiguousGrammarInfiniteNumberOfPathsTestCorrectness") {
    var S: Nonterminal[String] = null
    S = syn("a" ~ S ~ "b" | "a" ~ "b")

    val graph = Graph(
      (0 ~+#> 1)("a"),
      (1 ~+#> 2)("a"),
      (2 ~+#> 0)("a"),
      (0 ~+#> 3)("b"),
      (3 ~+#> 0)("b"))

    val trees = getTrees(graph, S)

    val rule1 = Rule(S.symbol, Sequence(TerminalSymbol("a"), S.symbol, TerminalSymbol("b")))
    val rule2 = Rule(S.symbol, Sequence(TerminalSymbol("a"), TerminalSymbol("b")))

    val minTree = RuleNode(rule2, Seq(terminalNode("a"), terminalNode("b")))
    val expectedTrees = Stream.iterate(minTree)(tree => RuleNode(rule1, Seq(terminalNode("a"), tree, terminalNode("b"))))
      .zipWithIndex.filter({case (_, index) => index % 3 == 2}).map({case (tree, _) => tree})

    expectedTrees.zip(trees).take(10).foreach {
      case (expected, actual) => compareTreesIgnoringExtents(expected, actual) shouldBe true}
  }

  test("AmbiguousGrammarTwoPathsTestQuantity") {
    var S: Nonterminal[String] = null
    S = syn("x" ~ S | S ~ "x" | "x" ~ "x")

    val graph = Graph(
      (0 ~+#> 1)("x"),
      (1 ~+#> 2)("x"),
      (2 ~+#> 3)("x"),
      (0 ~+#> 4)("x"),
      (4 ~+#> 5)("x"),
      (5 ~+#> 6)("x"))

    getTrees(graph, S).size shouldBe 6
  }

  def getTrees(graph: Graph[Int, LkDiEdge], S: Nonterminal[String]): Stream[Tree] =
    SPPFsToTrees(getSPPFs(S, new GraphxInput(graph)).right.getOrElse(null)._1)
}
