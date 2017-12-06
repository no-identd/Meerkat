package org.meerkat

import org.meerkat.parsers.{AbstractCPSParsers, Layout, SPPFStatistics, Trampoline}
import org.meerkat.sppf.{DefaultSPPFLookup, NonPackedNode, NonterminalNode}
import org.meerkat.util.{IGraph, InputGraph}

import scala.language.reflectiveCalls

package object graph {
  def parseGraphFromAllPositions(parser: AbstractCPSParsers.AbstractSymbol[_, _],
                                 graph: IGraph,
                                 nontermsOpt: Option[List[String]] = None): Seq[NonterminalNode] = {
    val sppfLookup = new DefaultSPPFLookup(graph)
    val nodesCount = graph.nodesCount
    for (i <- 0 until nodesCount) {
      parser.reset()
      Layout.LAYOUT.get.reset()
      val input = new InputGraph(graph, i)
      parser(input, i, sppfLookup)(t => {})
      Trampoline.run
    }
    nontermsOpt
      .getOrElse(List(parser.name))
      .flatMap(sppfLookup.findNonterminalsByName)
  }
}
