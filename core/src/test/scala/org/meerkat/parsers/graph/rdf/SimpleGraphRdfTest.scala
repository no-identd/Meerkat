package org.meerkat.parsers.graph.rdf

import org.meerkat.util.{IGraph, SimpleGraph}

import scalax.collection.Graph
import scalax.collection.edge.Implicits._

class SimpleGraphRdfTest extends RdfTest("SimpleGraph") {
  override def edgesToGraph(edges: List[(Int, String, Int)], nodesCount: Int): SimpleGraph = {
    val scalaxEdges = edges
      .map { case (f, l, t) =>
        (f  ~+#> t)(l)
      }
    IGraph(Graph(scalaxEdges :_*))
  }
}
