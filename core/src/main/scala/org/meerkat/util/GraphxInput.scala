package org.meerkat.util

import scalax.collection.Graph
import scalax.collection.edge.LkDiEdge

class GraphxInput(graph: Graph[Int, LkDiEdge]) extends Input {
  override def length: Int = graph.order

  override def filterEdges(nodeId: Int, label: String): collection.Seq[Int] =
    graph.get(nodeId)
      .outgoing
      .collect {
        case e if e.label.toString == label => e.to.value
      }
      .toSeq

  override def outEdges(node: Int): collection.Seq[Edge] =
    graph
      .get(node)
      .outgoing
      .map(e => (e.label.toString, e.to.value))
      .toSeq

  override def checkNode(id: Int, label: String): Boolean =
    true

  override def substring(start: Int, end: Int): String =
    throw new RuntimeException("Can not be done for graphs")

}

object GraphxInput {
  def apply(graph: Graph[Int, LkDiEdge]): GraphxInput = new GraphxInput(graph)
}