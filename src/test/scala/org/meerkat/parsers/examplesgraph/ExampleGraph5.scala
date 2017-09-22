package org.meerkat.parsers.examplesgraph

import org.meerkat.Syntax._
import org.meerkat.parsers.Parsers._
import org.meerkat.parsers._
import org.meerkat.util.IGraph

import scalax.collection.Graph
import scalax.collection.edge.Implicits._

/**
  * Created by sofysmo on 27.11.16.
  */
object ExampleGraph5 {
  val E: Nonterminal
  = syn ( "a"~~"b" ~~ E
    | "a"~~"b" )
  val g = Graph((0~+#>1)("a"),(1~+#>0)("b"))
  def main(args: Array[String]): Unit = {
    getResult(E,IGraph(g),"myGraph5")
  }
}
