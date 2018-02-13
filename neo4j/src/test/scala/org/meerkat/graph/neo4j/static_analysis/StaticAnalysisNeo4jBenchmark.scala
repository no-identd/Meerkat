package org.meerkat.graph.neo4j.static_analysis

import org.meerkat.graph.neo4j.edgesToNeo4jGraph
import org.meerkat.graph.static_analysis.StaticAnalysisMixin

object StaticAnalysisNeo4jBenchmark extends App with StaticAnalysisMixin {
  val times = 1
  val results = benchmark(times, edgesToNeo4jGraph)
  for ((file, time) <- results) {
    println(s"$file, $time")
  }
}
