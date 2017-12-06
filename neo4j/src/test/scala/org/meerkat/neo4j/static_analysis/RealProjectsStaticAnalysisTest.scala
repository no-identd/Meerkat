package org.meerkat.neo4j.static_analysis

import org.neo4j.graphdb.{GraphDatabaseService, Label}

class RealProjectsStaticAnalysisTest extends StaticAnalysisTest {
  override def fillDb(db: GraphDatabaseService): Unit = {
    val Some((nodes, edges)) =
      parseJsonGraph(getClass.getResource("/static_analysis/gzip.json").getFile)
    val dbNodes = nodes
      .mapValues(l => db.createNode(Label.label(l)))
    edges.foreach { case (f, t, l) =>
      dbNodes(f).createRelationshipTo(dbNodes(t), () => l)
    }
  }
}
