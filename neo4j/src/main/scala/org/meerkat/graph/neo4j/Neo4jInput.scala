package org.meerkat.graph.neo4j

import org.meerkat.input.Input
import org.neo4j.graphdb.{Direction, GraphDatabaseService, Node, Relationship}

import scala.collection.JavaConverters._

class Neo4jInput(db: GraphDatabaseService) extends Input[String, String] {
  private val internalIdToDbId =
    db.getAllNodes.asScala
      .map(_.getId)
      .zipWithIndex
      .map(_.swap)
      .toMap

  private val dbIdToInternalId =
    internalIdToDbId.map(_.swap)


  override def edgesCount: Int =
    internalIdToDbId.size

  override def filterEdges(nodeId: Int, predicate: String => Boolean): Seq[(String, Int)] =
    db.getNodeById(internalIdToDbId(nodeId))
      .getRelationships(Direction.OUTGOING)
      .asScala
      .collect {
        case r if predicate(r.getType.name) =>
          (r.getType.name, dbIdToInternalId(r.getEndNodeId))
      }
      .toSeq

  override def checkNode(nodeId: Int, predicate: String => Boolean): Boolean =
    db.getNodeById(internalIdToDbId(nodeId))
      .getLabels
      .asScala
      .map(_.name)
      .exists(predicate)
}
