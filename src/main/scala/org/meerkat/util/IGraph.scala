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
  def getFromNode: INode

  def getToNode: INode

  def getLabel: String
}

trait INode {
  def getOutgoingEdges: Set[IEdge]

  def getIncomingEdges: Set[IEdge]

  def value: Int
}

trait IGraph {
  def countNodes: Int

  def get(n: Int): INode
}

object IGraph {
  def apply(s: Graph[Int, LkDiEdge]) = new SimpleGraph(s)

  implicit def toInput(s: Graph[Int, LkDiEdge]) = Graph(s)
}

class SimpleGraph(val graph: Graph[Int, LkDiEdge]) extends IGraph {

  class Node(val value: Int) extends INode {
    val node = graph.get(value)

    def getOutgoingEdges: Set[IEdge]
    = node.outgoing.map(edge =>
      new Edge(new Node(edge.from.value), new Node(edge.to.value), edge.label.toString))

    def getIncomingEdges: Set[IEdge]
    = node.incoming.map(edge => new Edge(new Node(edge.from.value), new Node(edge.to.value), edge.label.toString))

    //def isPredecessorOf(n: INode): Boolean
    //= node isPredecessorOf graph.get(n.value)

  }

  class Edge(from: Node, to: Node, label: String) extends IEdge {
    def getFromNode: INode = from

    def getToNode: INode = to

    def getLabel: String = label
  }

  def countNodes: Int = graph.order

  def get(n: Int): INode = new Node(n)
}

case class Relationship(start: String, end: String, `type`: String)

class Neo4jGraph(url: String, login: String, password: String) extends IGraph {

  private val client = new HttpClient(login, password)
  class Node(val value: Int) extends INode {
    val node = value
    def getOutgoingEdges: Set[IEdge] = getEdge(s"$url/db/data/node/$value/relationships/out")


    def getIncomingEdges: Set[IEdge] = getEdge(s"$url/db/data/node/$value/relationships/in")

    private def getEdge(url: String): Set[IEdge] = {
      implicit val formats = Serialization.formats(NoTypeHints)
      JsonUtil.fromJson[scala.collection.Seq[Relationship]](client.get(url)).map(rel => {
      val i = rel.start.lastIndexOf("/")
      val start = Integer.parseInt(rel.start.substring(i+1, rel.start.length))
      val end = Integer.parseInt(rel.end.substring(i+1, rel.end.length))
      new Edge(new Node(start), new Node(end),rel.`type`)}).toSet.asInstanceOf[Set[IEdge]]
    }
  }

  class Edge(from: Node, to: Node, label: String) extends IEdge {
    def getFromNode: INode = from

    def getToNode: INode = to

    def getLabel: String = label
  }

  def countNodes: Int = 10

  def get(n: Int): INode = new Node(n)
}
