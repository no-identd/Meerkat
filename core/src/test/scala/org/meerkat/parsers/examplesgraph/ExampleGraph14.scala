package org.meerkat.parsers.examplesgraph

/**
  * Created by sofysmo on 06.12.16.
  */
import org.meerkat.Syntax._
import org.meerkat.parsers.Parsers._
import org.meerkat.util.IGraph
import org.meerkat.parsers._

import scalax.collection.Graph
import scalax.collection.edge.Implicits._

object ExampleGraph14 {
  val A = syn ( "a" ~~ "b" )
  val B: Nonterminal
  = syn ("d"
    | B ~~ "d")
  val E: Nonterminal
  = syn ( A ~~ E ~~ B
    | "d" )
  val g = Graph((0~+#>1)("a"),(1~+#>2)("b"),
    (0~+#>3)("d"),(3~+#>2)("a"),
    (2~+#>4)("d"),(4~+#>2)("d"))
  def main(args: Array[String]): Unit = {
    getSppfWithStartsAndEnds(E,IGraph(g),Set(2),Set(4), "myGraph14")
  }
}
