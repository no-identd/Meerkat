package org.meerkat.util

/**
 * Created by sofysmo on 04.12.16.
 */
import scala.collection.immutable.Set
import scalax.collection.Graph
import scalax.collection.edge.LkDiEdge

class SimpleGraphInput(graph: Graph[Int, LkDiEdge]) extends Input[LkDiEdge[_], Int] {

//  override def nodesCount: Int = graph.order
//
//  override def get(n: Int): INode = new Node(n)
//
//  class Node(val value: Int) extends INode {
//    private val node = graph.get(value)
//
//    def outgoingEdges: Set[IEdge] = node.outgoing.map { edge =>
//      new Edge(new Node(edge.from.value), new Node(edge.to.value), edge.label.toString)
//    }
//
//    def incomingEdges: Set[IEdge] = node.incoming.map { edge =>
//      new Edge(new Node(edge.from.value), new Node(edge.to.value), edge.label.toString)
//    }
//  }
//
//  class Edge(val from: Node, val to: Node, val label: String) extends IEdge
  override def length: Int = graph.order

  override def filterEdges(node: Int, label: String): collection.Seq[Int] =
    graph.get(node).outgoing
      .collect {
        case e if e.label.toString == label => e.to.value
      }.toSeq


  override def idByNode(node: Int): Int = node

  override def nodeById(id: Int): Int = id
}
