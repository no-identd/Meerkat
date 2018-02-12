package org.meerkat.parsers.graph.rdf

import org.scalatest.FunSuite
import org.scalatest.Matchers._


class RdfTest extends FunSuite with RdfMixin {
  test("RDF in memory graph test") {
    getResults(edgesToInMemoryGraph) should contain theSameElementsAs rdfs
  }
}
