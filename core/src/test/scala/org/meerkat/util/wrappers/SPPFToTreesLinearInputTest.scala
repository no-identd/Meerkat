package org.meerkat.util.wrappers

import org.meerkat.Syntax._
import org.meerkat.input.LinearInput
import org.meerkat.parsers.Parsers._
import org.meerkat.parsers._
import org.meerkat.tree._
import org.meerkat.util.wrappers.TestUtils._
import org.meerkat.util.wrappers.SPPFToTreesBFSTransformation._
import org.meerkat.sppf.SPPFNode
import org.scalatest.{FunSuite, Matchers}

import scala.collection.mutable

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

  test("InfiniteLoopTestSPPFNodeUniqueness") {
    var S: Nonterminal[Char] = null
    S = syn(S ~ S | 'x' | epsilon)

    val input = new LinearInput("x".toVector)
    val nodes = Stream.iterate(Seq[SPPFNode](extractNonAmbiguousSPPFs(
                  getSPPF(S, input).getOrElse(null)._1).drop(2).head))(
                    layer => layer.flatMap(node => node.children))
                .takeWhile(layer => !layer.isEmpty).flatten

    val set = mutable.Set[(SPPFNode, Int)]()
    nodes.foreach(node => {
      val key = (node, System.identityHashCode(node))
      set.contains(key) shouldBe false
      set.add(key)
    })
  }

  def getTrees(source: String, S: Nonterminal[Char]): Stream[Tree] = {
    val input = new LinearInput(source.toVector)
    extractTreesFromSPPF(getSPPF(S, input).getOrElse(null)._1)(input)
  }
}
