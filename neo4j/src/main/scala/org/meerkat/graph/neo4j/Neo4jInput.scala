package org.meerkat.graph.neo4j

import org.meerkat.util.Input
import org.neo4j.graphdb.{Direction, GraphDatabaseService, Node, Relationship}

import scala.collection.JavaConverters._


class Neo4jInput(db: GraphDatabaseService)
  extends Input[Relationship, Node] {
  private val internalIdToDbId =
    db.getAllNodes.asScala
      .map(_.getId)
      .zipWithIndex
      .map(_.swap)
      .toMap
  private val dbIdToInternalId =
    internalIdToDbId.map(_.swap)

  override def filterEdges(node: Node, label: String): Seq[Int] =
    node.getRelationships(Direction.OUTGOING)
      .asScala
      .filter(_.getType.name() == label)
        .map(r => dbIdToInternalId(r.getEndNodeId))
      .toSeq

  override def idByNode(node: Node): Int =
    dbIdToInternalId(node.getId)

//  override def to(edge: Relationship): Node =
//    edge.getEndNode
  override def length: Int = internalIdToDbId.size

  override def nodeById(id: Int): Node = db.getNodeById(internalIdToDbId(id))
}