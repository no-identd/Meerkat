package org.meerkat.util


/**
  * Created by sofysmo on 04.12.16.
  */

import org.json4s.NoTypeHints
import org.json4s.jackson.Serialization

import scalax.collection.Graph
import scalax.collection.edge.{LDiEdge, LkDiEdge}
import scala.collection.immutable.Set

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

case class Relationship(start: String, end: String, `type`: String)

class Neo4jGraph(url: String, login: String, password: String) extends IGraph {

  private val client = new HttpClient(login, password)

  def nodesCount: Int = 10

  def get(n: Int): INode = new Node(n)

  class Node(val value: Int) extends INode {
    val node = value

    def outgoingEdges: Set[IEdge] = getEdge(s"$url/db/data/node/$value/relationships/out")

    private def getEdge(url: String): Set[IEdge] = {
      implicit val formats = Serialization.formats(NoTypeHints)
      JsonUtil.fromJson[scala.collection.Seq[Relationship]](client.get(url)).map(rel => {
        val i = rel.start.lastIndexOf("/")
        val start = Integer.parseInt(rel.start.substring(i + 1, rel.start.length))
        val end = Integer.parseInt(rel.end.substring(i + 1, rel.end.length))
        new Edge(new Node(start), new Node(end), rel.`type`)
      }).toSet.asInstanceOf[Set[IEdge]]
    }

    def incomingEdges: Set[IEdge] = getEdge(s"$url/db/data/node/$value/relationships/in")
  }

  class Edge(val from: Node, val to: Node, val label: String) extends IEdge

}
