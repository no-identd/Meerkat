package org.meerkat.graph.neo4j

import org.meerkat.graph.neo4j.Neo4jInput.Entity
import org.meerkat.input.Input
import org.neo4j.graphdb._

import scala.collection.JavaConverters._
import scala.language.{dynamics, implicitConversions}


class Neo4jInput(db: GraphDatabaseService) extends Input[Entity, Entity] {
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

  override def filterEdges(nodeId: Int, predicate: Entity => Boolean): Seq[(Entity, Int)] =
    db.getNodeById(internalIdToDbId(nodeId))
      .getRelationships(Direction.OUTGOING)
      .asScala
      .collect {
        case r if predicate(Entity(r)) =>
          (Entity(r), dbIdToInternalId(r.getEndNodeId))
      }
      .toSeq

  override def checkNode(nodeId: Int, predicate: Entity => Boolean): Option[Entity] = {
    val property =
      Entity(db.getNodeById(internalIdToDbId(nodeId)))
    Some(property).filter(predicate)
  }
}

object Neo4jInput {

  class Entity(entity: PropertyContainer) extends Dynamic {
    def value(): String = entity match {
      case node: Node =>
        node.getLabels.asScala.head.name()
      case relationship: Relationship =>
        relationship.getType.name()
      case _ => ""
    }

    def selectDynamic[T](name: String): T =
      entity.getProperty(name).asInstanceOf[T]

  }

  object Entity {
    def apply(entity: PropertyContainer): Entity = new Entity(entity)
  }

  implicit def toPredicate(predicate: String => Boolean): (Entity => Boolean) =
    (p: Entity) => predicate(p.value())

  implicit def toPredicate(label: String): (Entity => Boolean) =
    (p: Entity) => p.value() == label

  implicit def entityInputToStringInput(neo4jInput: Neo4jInput): Input[String, String] =
    new Input[String, String] {
      override def edgesCount: Int = neo4jInput.edgesCount

      override def filterEdges(nodeId: Int, predicate: String => Boolean): Seq[(String, Int)] =
        neo4jInput
          .filterEdges(nodeId, x => predicate(x.value()))
          .map { case (e, i) => (e.value(), i) }

      override def checkNode(nodeId: Int, predicate: String => Boolean): Option[String] =
        neo4jInput
          .checkNode(nodeId, x => predicate(x.value()))
          .map(_.value())
    }
}

