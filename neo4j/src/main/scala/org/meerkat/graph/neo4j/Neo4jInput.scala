package org.meerkat.graph.neo4j

import org.meerkat.util.Input
import org.neo4j.graphdb.{Direction, GraphDatabaseService}

import scala.collection.JavaConverters._

class Neo4jInput(db: GraphDatabaseService) extends Input {
  private val internalIdToDbId =
    db.getAllNodes.asScala
      .map(_.getId)
      .zipWithIndex
      .map(_.swap)
      .toMap

  private val dbIdToInternalId =
    internalIdToDbId.map(_.swap)

  override def filterEdges(nodeId: Int, label: String): Seq[Int] =
    db.getNodeById(internalIdToDbId(nodeId))
      .getRelationships(Direction.OUTGOING)
      .asScala
      .collect {
        case r if r.getType.name() == label => dbIdToInternalId(r.getEndNodeId)
      }
      .toSeq

  override def length: Int =
    internalIdToDbId.size

  override def outEdges(nodeId: Int): Seq[Edge] =
    db.getNodeById(internalIdToDbId(nodeId))
      .getRelationships(Direction.OUTGOING)
      .asScala
      .map(r => (r.getType.name, dbIdToInternalId(r.getEndNodeId)))
      .toSeq

  override def checkNode(nodeId: Int, label: String): Boolean =
    db.getNodeById(internalIdToDbId(nodeId))
      .getLabels
      .asScala
      .exists(_.name == label)

  override def substring(start: Int, end: Int): String =
    throw new RuntimeException("Can not be done for graphs")
}
