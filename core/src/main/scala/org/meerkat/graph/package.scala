package org.meerkat

import org.meerkat.parsers.{AbstractCPSParsers, Layout, Trampoline}
import org.meerkat.sppf.{DefaultSPPFLookup, NonterminalNode}
import org.meerkat.util._

import scala.language.reflectiveCalls
import scalax.collection.Graph
import scalax.collection.edge.Implicits._

package object graph {
  def parseGraphFromAllPositions(parser: AbstractCPSParsers.AbstractSymbol[_, _],
                                 graph: Input[_,_],
                                 nontermsOpt: Option[List[String]] = None): collection.Seq[NonterminalNode] = {
    val sppfLookup = new DefaultSPPFLookup(graph)
    val nodesCount = graph.length
    parser.reset()
    Layout.LAYOUT.get.reset()
    for (i <- 0 until nodesCount) {
      parser(graph, i, sppfLookup)(t => {})
      Trampoline.run
    }
    nontermsOpt
      .getOrElse(List(parser.name))
      .flatMap(sppfLookup.findNonterminalsByName)
  }

  def edgesToInMemoryGraph(edges: List[(Int, String, Int)], nodesCount: Int): SimpleGraphInput = {
    val scalaxEdges = edges.map {
      case (f, l, t) =>
        (f ~+#> t)(l)
    }
    new SimpleGraphInput(Graph(scalaxEdges: _*))
  }

}
