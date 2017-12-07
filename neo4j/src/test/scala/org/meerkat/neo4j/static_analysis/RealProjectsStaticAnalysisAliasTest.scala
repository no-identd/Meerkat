package org.meerkat.neo4j.static_analysis

import org.neo4j.graphdb.{GraphDatabaseService, Label, Node}

import scala.collection.mutable

class RealProjectsStaticAnalysisAliasTest extends StaticAnalysisAliasTest {
  override def fillDb(db: GraphDatabaseService): Unit = {
    val Some((nodes, edges)) =
      parseJsonGraph(getClass.getResource("/static_analysis/gzip.json").getFile)
    val dbNodes = mutable.Map[Int, Node]() // because of laziness of immutable Map.map
    for ((k, v) <- nodes) {
      dbNodes += k -> db.createNode(Label.label(v))
    }
    edges.foreach { case (f, t, l) =>
      dbNodes(f).createRelationshipTo(dbNodes(t), () => l)
    }
  }
}
