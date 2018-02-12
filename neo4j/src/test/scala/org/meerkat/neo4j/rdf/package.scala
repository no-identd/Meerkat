package org.meerkat.neo4j

import org.neo4j.test.TestGraphDatabaseFactory

package object rdf {
  def edgesToNeo4jGraph(edges: List[(Int, String, Int)], nodesCount: Int): EmbeddedNeo4jGraph = {
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
