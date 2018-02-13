package org.meerkat.util

/**
 * Created by sofysmo on 04.12.16.
 */
import scala.collection.immutable.Set
import scalax.collection.Graph
import scalax.collection.edge.LkDiEdge

trait IEdge {
  def from: INode

  def to: INode

  def label: String
}

trait INode {
  def outgoingEdges: Set[IEdge]

  def incomingEdges: Set[IEdge]

  def value: Int
}

trait IGraph {
  def nodesCount: Int

  def get(n: Int): INode
}

object IGraph {
  def apply(s: Graph[Int, LkDiEdge]) = new SimpleGraph(s)

  implicit def toInput(s: Graph[Int, LkDiEdge]) = Graph(s)
}

class SimpleGraph(graph: Graph[Int, LkDiEdge]) extends IGraph {

  override def nodesCount: Int = graph.order

  override def get(n: Int): INode = new Node(n)

  class Node(val value: Int) extends INode {
    private val node = graph.get(value)

    def outgoingEdges: Set[IEdge] = node.outgoing.map { edge =>
      new Edge(new Node(edge.from.value), new Node(edge.to.value), edge.label.toString)
    }

    def incomingEdges: Set[IEdge] = node.incoming.map { edge =>
      new Edge(new Node(edge.from.value), new Node(edge.to.value), edge.label.toString)
    }
  }

  class Edge(val from: Node, val to: Node, val label: String) extends IEdge

}
