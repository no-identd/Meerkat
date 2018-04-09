package org.meerkat.util.wrappers

import org.meerkat.Syntax._
import org.meerkat.input.LinearInput
import org.meerkat.parsers.Parsers._
import org.meerkat.parsers._
import org.meerkat.tree._
import org.meerkat.util.wrappers.TestUtils._
import org.scalatest.{FunSuite, Matchers}

class SPPFToTreesLinearInputTest extends FunSuite with Matchers {

  test("NonAmbiguousGrammarTestQuantity") {
    var S: Nonterminal[Char] = null
    S = syn(S ~ '-' ~ 'x' | S ~ '+' ~ 'x' | 'x')

    getTrees("x", S).size shouldBe 1
    getTrees("x+x-x-x+x", S).size shouldBe 1
  }

  test("NonAmbiguousGrammarTestCorrectness") {
    var S: Nonterminal[Char] = null
    S = syn(S ~ '+' ~ 'x' | 'x')

    val tree = getTrees("x+x+x", S).head

    val rule1 = Rule(S.symbol, TerminalSymbol('x'))
    val rule2 = Rule(S.symbol, Sequence(S.symbol, TerminalSymbol('+'), TerminalSymbol('x')))
    compareTreesIgnoringExtents(tree,
      RuleNode(rule2, Seq(
        RuleNode(rule2, Seq(
          RuleNode(rule1, Seq(
           terminalNode("x"))),
          terminalNode("+"),
          terminalNode("x"))),
        terminalNode("+"),
        terminalNode("x")))) shouldBe true
  }

  test("AmbiguousGrammarTestQuantity") {
    var S: Nonterminal[Char] = null
    S = syn(S ~ '+' ~ S | 'x')

    getTrees("x+x+x", S).size shouldBe 2
  }

  test("AmbiguousGrammarWithInfiniteLoopTestOrder") {
    var S: Nonterminal[Char] = null
    S = syn(S ~ S | 'x' | epsilon)

    val count = 20
    val sizes = getTrees("x", S).take(count).map(treeSize)
    sizes.zip(Stream(0) ++ sizes).count {case (a, b) => (a >= b)} shouldBe count
  }

  def getTrees(input: String, S: Nonterminal[Char]): Stream[Tree] =
    SPPFToTrees(getSPPF(S, new LinearInput(input.toVector, "")).getOrElse(null)._1)

}
