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
object ExampleGraph7 {
  val E: Nonterminal = syn ( "(" ~~ E ~~ ")"
    | "N")
  val S = syn(E)
  val g = Graph((0~+#>1)("("),(1~+#>2)("N"),(2~+#>3)(")"))

  def main(args: Array[String]): Unit = {
    getResult(S,IGraph(g),"myGraph7")
  }
}
