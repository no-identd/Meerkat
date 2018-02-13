package org.meerkat.graph.rdf

import org.meerkat.graph.edgesToInMemoryGraph

object RdfBenchmark extends App with RdfMixin {
  val times   = 10
  val results = benchmark(times, edgesToInMemoryGraph)
  for ((file, time1, time2) <- results) {
    println(s"$file, $time1, $time2")
  }
}
