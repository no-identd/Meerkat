package org.meerkat.neo4j

import scala.io.Source
import scala.util.parsing.json.JSON

package object static_analysis {
  def parseJsonGraph(filename: String): Option[(Map[Int, String], List[(Int, Int, String)])] =
    JSON.parseFull(Source.fromFile(filename).mkString).map { json =>
      val map = json.asInstanceOf[Map[String, Any]]
      val nodes = map("nodes").asInstanceOf[List[Map[String, Any]]]
        .map(n => (n("id").asInstanceOf[Double].toInt, n("value").asInstanceOf[String]))
        .toMap
      val edges = map("edges").asInstanceOf[List[Map[String, Any]]]
        .map { n =>
          (n("from").asInstanceOf[Double].toInt,
            n("to").asInstanceOf[Double].toInt,
            n("label").asInstanceOf[String])
        }
      (nodes, edges)
    }
}
