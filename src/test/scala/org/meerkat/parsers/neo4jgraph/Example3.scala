package org.meerkat.parsers.neo4jgraph

import org.meerkat.Syntax._
import org.meerkat.parsers.Parsers._
import org.meerkat.parsers.examplesgraph._
import org.meerkat.util.{Input, Neo4jGraph}
import org.meerkat.parsers._

/**
  * Created by sofysmo on 25.03.17.
  */
object Example3 {

  val E: Nonterminal = syn ( "A" ~~ E ~~ "B"
    | epsilon)
  def main(args: Array[String]): Unit = {
    val graph = new Neo4jGraph("http://localhost:7474","neo4j","karamell111")
    getResult(E,Input(graph, 7), "neo4j_3")
  }
}
