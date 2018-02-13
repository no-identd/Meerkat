package org.meerkat.parsers.graph.static_analysis

import org.meerkat.parsers.graph.edgesToInMemoryGraph

object StaticAnalysisBenchmark extends App with StaticAnalysisMixin {
  val times = 1
  val results = benchmark(times, edgesToInMemoryGraph)
  for ((file, time) <- results) {
    println(s"$file, $time")
  }
}
