package org.meerkat.input

import scalax.collection.Graph
import scalax.collection.edge.LkDiEdge

class GraphxInput[L](graph: Graph[Int, LkDiEdge]) extends Input[L, Nothing] {

  override def edgesCount: Int = graph.order

  override def filterEdges(nodeId: Int, predicate: L => Boolean): collection.Seq[(L, Int)] =
    graph.get(nodeId)
      .outgoing
      .collect {
        case e if predicate(e.label.asInstanceOf[L]) => (e.label.asInstanceOf[L], e.to.value)
      }
      .toSeq

  override def checkNode(nodeId: Int, predicate: Nothing => Boolean): Boolean =
    true
}

object GraphxInput {
  def apply[L](graph: Graph[Int, LkDiEdge]): GraphxInput[L] = new GraphxInput(graph)
}