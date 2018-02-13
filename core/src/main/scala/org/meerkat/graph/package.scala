package org.meerkat

import org.meerkat.parsers.{AbstractCPSParsers, Layout, Trampoline}
import org.meerkat.sppf.{DefaultSPPFLookup, NonterminalNode}
import org.meerkat.util.{IGraph, InputGraph, SimpleGraph}

import scala.language.reflectiveCalls
import scalax.collection.Graph
import scalax.collection.edge.Implicits._

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

  def edgesToInMemoryGraph(edges: List[(Int, String, Int)], nodesCount: Int): SimpleGraph = {
    val scalaxEdges = edges.map {
      case (f, l, t) =>
        (f ~+#> t)(l)
    }
    IGraph(Graph(scalaxEdges: _*))
  }

}
