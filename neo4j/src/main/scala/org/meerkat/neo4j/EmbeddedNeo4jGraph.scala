package org.meerkat.neo4j

import org.meerkat.util.{IEdge, IGraph, INode}
import org.neo4j.graphdb.{Direction, GraphDatabaseService, Node}

import scala.collection.JavaConverters._
import scala.collection.immutable.Set

// Assumes that graph can not be changed since the instance being used
class EmbeddedNeo4jGraph(db: GraphDatabaseService) extends IGraph {
  private val internalIdToDbId =
    db.getAllNodes.asScala
      .map(_.getId)
      .zipWithIndex
      .map(_.swap)
      .toMap
  private val dbIdToInternalId =
    internalIdToDbId.map(_.swap)

  override def nodesCount: Int =
    internalIdToDbId.size

  override def get(n: Int): INode = {
    EmbeddedNeo4jGraph.toINode(db.getNodeById(internalIdToDbId(n)))(dbIdToInternalId)
  }
}

object EmbeddedNeo4jGraph {
  def toINode(node: Node)(implicit nodes: Map[Long, Int]): INode =
    new INode {
      override def outgoingEdges: Set[IEdge] =
        node.getRelationships(Direction.OUTGOING)
          .asScala
          .map(toIEdge)
          .toSet

      override def incomingEdges: Set[IEdge] =
        node.getRelationships(Direction.INCOMING)
          .asScala
          .map(toIEdge)
          .toSet

      override def value: Int = nodes(node.getId)
    }

  def toIEdge(relationship: org.neo4j.graphdb.Relationship)(implicit nodes: Map[Long, Int]): IEdge =
    new IEdge {
      override def label: String = relationship.getType.name

      override def from: INode = toINode(relationship.getStartNode)

      override def to: INode = toINode(relationship.getEndNode)
    }
}