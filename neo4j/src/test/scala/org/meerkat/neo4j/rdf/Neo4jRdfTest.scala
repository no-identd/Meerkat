package org.meerkat.neo4j.rdf

import org.meerkat.parsers.graph.rdf.RdfMixin
import org.meerkat.neo4j.edgesToNeo4jGraph
import org.scalatest.FunSuite
import org.scalatest.Matchers._

class Neo4jRdfTest extends FunSuite with RdfMixin {
  test("RDF neo4j graph test") {
    getResults(edgesToNeo4jGraph) should contain theSameElementsAs rdfs
  }
}
