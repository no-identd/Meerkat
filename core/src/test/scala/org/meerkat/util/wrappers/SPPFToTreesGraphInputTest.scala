package org.meerkat.util.wrappers

import org.meerkat.Syntax._
import org.meerkat.input.GraphxInput
import org.meerkat.parsers.Parsers.{Nonterminal, _}
import org.meerkat.parsers._
import org.meerkat.tree._
import org.meerkat.util.visualization.{visualize, toDot}
import org.scalactic.Prettifier
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

    val trees = getForests(graph, S)(0)

    trees.size shouldBe 2

    val rule1 = Rule(S.symbol, Sequence(TerminalSymbol("x"), TerminalSymbol("+"), TerminalSymbol("x")))
    val rule2 = Rule(S.symbol, Sequence(TerminalSymbol("x"), TerminalSymbol("-"), TerminalSymbol("x")))

    Prettifier.default(trees(0)) shouldBe Prettifier.default(
      RuleNode(rule1, Seq(TerminalNode("x"), TerminalNode("+"), TerminalNode("x")))
    )

    Prettifier.default(trees(1)) shouldBe Prettifier.default(
      RuleNode(rule2, Seq(TerminalNode("x"), TerminalNode("-"), TerminalNode("x")))
    )
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

    val trees0 = getForests(graph, S)(0)
    val trees1 = getForests(graph, S)(1)

    val rule1 = Rule(S.symbol, Sequence(TerminalSymbol("a"), S.symbol, TerminalSymbol("b")))
    val rule2 = Rule(S.symbol, Sequence(TerminalSymbol("a"), TerminalSymbol("b")))

    val minTree = RuleNode(rule2, Seq(TerminalNode("a"), TerminalNode("b")))
    val expectedTrees = Stream.iterate(minTree)(tree => RuleNode(rule1, Seq(TerminalNode("a"), tree, TerminalNode("b"))))
      .zipWithIndex

    val expectedTrees0 = expectedTrees.filter({case (_, index) => index % 6 == 2}).map({case (tree, _) => tree})
    val expectedTrees1 = expectedTrees.filter({case (_, index) => index % 6 == 5}).map({case (tree, _) => tree})

    expectedTrees0.zip(trees0).take(5).foreach {
      case (expected, actual) => Prettifier.default(expected) shouldBe Prettifier.default(actual)}
    expectedTrees1.zip(trees1).take(5).foreach {
      case (expected, actual) => Prettifier.default(expected) shouldBe Prettifier.default(actual)}
  }

  test("AmbiguousGrammarTwoPathsTestQuantity") {
    var S: Nonterminal[String] = null
    S = syn("x" ~ S | S ~ "x" | "x")

    val graph = Graph(
      (0 ~+#> 1)("x"),
      (1 ~+#> 2)("x"),
      (2 ~+#> 3)("x"),
      (0 ~+#> 4)("x"),
      (4 ~+#> 5)("x"),
      (5 ~+#> 6)("x"))

    getForests(graph, S).map(_.size).max shouldBe 4
  }

  def getForests(graph: Graph[Int, LkDiEdge], S: Nonterminal[String]): List[Stream[Tree]] =
    getSPPFs(S, new GraphxInput(graph)).right.getOrElse(null)._1.map(SPPFToTrees)
}
