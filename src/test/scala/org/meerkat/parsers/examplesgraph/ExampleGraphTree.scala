package org.meerkat.parsers.examplesgraph

import org.meerkat.parsers.Parsers.SequenceBuilder
import org.meerkat.parsers.~

import org.meerkat.Syntax._
import org.meerkat.parsers.Parsers._
import org.meerkat.parsers._
import org.meerkat.util.IGraph

import scalax.collection.Graph
import scalax.collection.edge.Implicits._

object ExampleGraphTree {
  val toStr: String => String = x => x
  val A = syn { "ab" ^ toStr }
  val B = syn { "d" ^ toStr }

  val AB: SequenceBuilder[String~String] = A ~~ B
  val BA: SequenceBuilder[String~String] = B ~~ A
  val S = syn ( A ~~ B | B ~~ A )
  val g = Graph((0~+#>1)('a'), (0~+#>3)('d'),
    (1~+#>2)('b'), (2~+#>8)('d'), (3~+#>4)('a'), (4~+#>7)('b'),
    (0~+#>5)('b'),(5~+#>6)('d'))
  def main(args: Array[String]): Unit = {

    getResult(S, IGraph(g), "myGraphTree")
  }

}
/*
val E = syn ( "EATS" )
    val graph = new Neo4jGraph("http://localhost:7474","neo4j","karamell111")
    getResult(E,Input(graph, 1), "neo4j_1")
    http://localhost:7474/db/data/node/1/relationships/out
 */