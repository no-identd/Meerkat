package org.meerkat.neo4j

import org.meerkat.parsers.graph.rdf.RdfTest
import org.neo4j.test.TestGraphDatabaseFactory

class Neo4jRdfTest extends RdfTest("Neo4j") {
  override def edgesToGraph(edges: List[(Int, String, Int)], nodesCount: Int): EmbeddedNeo4jGraph = {
    val db = new TestGraphDatabaseFactory().newImpermanentDatabase
    db.beginTx()
    val nodes = List.fill(nodesCount)(db.createNode)
    edges
      .foreach {case (f, l, t) =>
          nodes(f).createRelationshipTo(nodes(t), () => l)
      }
    new EmbeddedNeo4jGraph(db)
  }
}
