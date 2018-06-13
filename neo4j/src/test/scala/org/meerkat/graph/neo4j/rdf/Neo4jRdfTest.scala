package org.meerkat.graph.neo4j.rdf

import org.meerkat.graph.rdf.RdfMixin
import org.meerkat.graph.neo4j.Neo4jInput._
import org.meerkat.graph.neo4j.edgesToNeo4jGraph
import org.scalatest.FunSuite
import org.scalatest.Matchers._

class Neo4jRdfTest extends FunSuite with RdfMixin {
  test("RDF neo4j graph test") {
    getResults(edgesToNeo4jGraph) should contain theSameElementsAs rdfs
  }
}
