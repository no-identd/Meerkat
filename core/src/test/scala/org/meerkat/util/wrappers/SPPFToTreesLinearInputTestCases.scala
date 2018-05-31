package org.meerkat.util.wrappers

import org.meerkat.Syntax._
import org.meerkat.input.LinearInput
import org.meerkat.parsers.Parsers._
import org.meerkat.parsers._
import org.meerkat.tree._
import org.meerkat.util.wrappers.TestUtils._
import org.meerkat.sppf.SPPFNode
import org.scalatest.{FunSuite, Matchers}

import scala.collection.mutable

object SPPFToTreesLinearInputTestCases extends Matchers {
  def nonAmbiguousGrammarTestQuantity(converter: SPPFToTreesConverter): Unit = {
    var S: Nonterminal[Char, Nothing] = null
    S = syn(S ~ '-' ~ 'x' | S ~ '+' ~ 'x' | 'x')

    getTrees("x", S, converter).size shouldBe 1
    getTrees("x+x-x-x+x", S, converter).size shouldBe 1
  }

  def nonAmbiguousGrammarTestCorrectness(converter: SPPFToTreesConverter): Unit = {
    var S: Nonterminal[Char, Nothing] = null
    S = syn(S ~ '+' ~ 'x' | 'x')

    val tree = getTrees("x+x+x", S, converter).head

    val rule1 = Rule(S.symbol, EdgeSymbol('x'))
    val rule2 = Rule(S.symbol, Sequence(S.symbol, EdgeSymbol('+'), EdgeSymbol('x')))
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

  def ambiguousGrammarTestQuantity(converter: SPPFToTreesConverter): Unit = {
    var S: Nonterminal[Char, Nothing] = null
    S = syn(S ~ '+' ~ S | 'x')

    getTrees("x+x+x", S, converter).size shouldBe 2
  }

  def infiniteLoopTestSPPFNodeUniqueness(converter: SPPFToTreesConverter): Unit = {
    var S: Nonterminal[Char, Nothing] = null
    S = syn(S ~ S | 'x' | epsilon)

    val input = new LinearInput("x".toVector)
    val nodes = Stream.iterate(Seq[SPPFNode](extractNonAmbiguousSPPFs(
                  getSPPF(S, input).getOrElse(null)._1, converter).drop(10).head))(
                    layer => layer.flatMap(node => node.children))
                .takeWhile(layer => layer.nonEmpty).flatten

    val set = mutable.Set[(SPPFNode, Int)]()
    nodes.foreach(node => {
      val key = (node, System.identityHashCode(node))
      set.contains(key) shouldBe false
      set.add(key)
    })
  }

  def getTrees(source: String, S: Nonterminal[Char, Nothing], converter: SPPFToTreesConverter): Stream[Tree] = {
    val input = new LinearInput(source.toVector)
    extractTreesFromSPPF(getSPPF(S, input).getOrElse(null)._1, converter)(input)
  }
}
