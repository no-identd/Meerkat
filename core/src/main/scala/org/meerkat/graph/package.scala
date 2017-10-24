package org.meerkat

import org.meerkat.parsers.{AbstractCPSParsers, Layout, Trampoline}
import org.meerkat.sppf.{DefaultSPPFLookup, NonPackedNode}
import org.meerkat.util.{IGraph, InputGraph}

package object graph {
  def parseGraphFromAllPositions(parser: AbstractCPSParsers.AbstractSymbol[_, _], graph: IGraph): List[NonPackedNode] = {
    val sppfLookup = new DefaultSPPFLookup(graph)
    val nodesCount = graph.nodesCount
    (for (i <- 0 until nodesCount) yield {
      parser.reset()
      Layout.LAYOUT.get.reset()
      val input = new InputGraph(graph, i)
      parser(input, i, sppfLookup)(t => {})
      Trampoline.run
      sppfLookup.getStartNodes(parser, i, input.length).toList.flatten
    }).toList.flatten
  }
}
