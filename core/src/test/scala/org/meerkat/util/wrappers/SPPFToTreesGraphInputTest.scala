package org.meerkat.util.wrappers

import org.meerkat.Syntax._
import org.meerkat.input.GraphxInput
import org.meerkat.parsers.Parsers.{Nonterminal, _}
import org.meerkat.parsers._
import org.meerkat.tree._
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

  test("NonAmbiguousGrammarInfinityNumberOfPathsTestCorrectness") {
    var S: Nonterminal[String] = null
    S = syn(S ~ "x" | "x")

    val graph = Graph(
      (0 ~+#> 0)("x"))

    val trees = getForests(graph, S)(0)

    val rule1 = Rule(S.symbol, Sequence(S.symbol, TerminalSymbol("x")))
    val rule2 = Rule(S.symbol, TerminalSymbol("x"))

    val minTree = RuleNode(rule2, Seq(TerminalNode("x")))
    val expectedTrees = Stream.iterate(minTree)(tree => RuleNode(rule1, Seq(tree, TerminalNode("x"))))

    expectedTrees.zip(trees).take(10).foreach {
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
