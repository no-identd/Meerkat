package org.meerkat.parsers.graph.rdf

import org.meerkat.parsers.graph.edgesToInMemoryGraph

object RdfBenchmark extends App with RdfMixin {
  val times = 10
  val results = benchmark(times, edgesToInMemoryGraph)
  for ((file, time1, time2) <- results) {
    println(s"$file, $time1, $time2")
  }
}
