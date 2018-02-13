package org.meerkat.neo4j.static_analysis

import org.meerkat.neo4j.edgesToNeo4jGraph
import org.meerkat.parsers.graph.static_analysis.StaticAnalysisMixin

object StaticAnalysisBenchmark extends App with StaticAnalysisMixin {
  val times = 1
  val results = benchmark(times, edgesToNeo4jGraph)
  for ((file, time) <- results) {
    println(s"$file, $time")
  }
}
