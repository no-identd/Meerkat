package org.meerkat.graph

import org.neo4j.test.TestGraphDatabaseFactory

package object neo4j {
  def edgesToNeo4jGraph(edges: List[(Int, String, Int)], nodesCount: Int): Neo4jInput = {
    val db = new TestGraphDatabaseFactory().newImpermanentDatabase
    db.beginTx()
    val nodes = List.fill(nodesCount)(db.createNode)
    edges.foreach {
      case (f, l, t) =>
        nodes(f).createRelationshipTo(nodes(t), () => l)
    }
    new Neo4jInput(db)
  }
}
