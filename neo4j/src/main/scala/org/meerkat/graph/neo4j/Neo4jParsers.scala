package org.meerkat.graph.neo4j

import org.meerkat.graph.neo4j.Neo4jInput._
import org.meerkat.input.Input
import org.meerkat.parsers._
import org.meerkat.parsers.Parsers._
import org.meerkat.sppf.{NonPackedNode, SPPFLookup}
import org.meerkat.tree.VertexSymbol
import org.neo4j.graphdb.Label

object Neo4jParsers {
  def LV(labels: String*) = V((e: Entity) => labels.forall(e.hasLabel))
  def LE(label: String) = E((e: Entity) => e.label() == label)
}
