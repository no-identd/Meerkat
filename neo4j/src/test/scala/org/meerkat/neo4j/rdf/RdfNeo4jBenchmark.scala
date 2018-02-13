package org.meerkat.neo4j.rdf

import org.meerkat.parsers.graph.rdf.RdfMixin
import org.meerkat.neo4j.edgesToNeo4jGraph

object RdfNeo4jBenchmark extends App with RdfMixin {
  val times = 10
  val results = benchmark(times, edgesToNeo4jGraph)
  for ((file, time1, time2) <- results) {
    println(s"$file, $time1, $time2")
  }
}
