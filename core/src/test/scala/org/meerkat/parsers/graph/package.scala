package org.meerkat.parsers

import org.meerkat.util.{IGraph, SimpleGraph}

import scalax.collection.Graph
import scalax.collection.edge.Implicits._

package object graph {
  def edgesToInMemoryGraph(edges: List[(Int, String, Int)], nodesCount: Int): SimpleGraph = {
    val scalaxEdges = edges
      .map { case (f, l, t) =>
        (f  ~+#> t)(l)
      }
    IGraph(Graph(scalaxEdges :_*))
  }
}
