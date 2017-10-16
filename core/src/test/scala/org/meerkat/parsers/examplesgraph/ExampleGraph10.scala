package org.meerkat.parsers.examplesgraph

import org.meerkat.Syntax._
import org.meerkat.parsers.Parsers._
import org.meerkat.parsers._
import org.meerkat.util.IGraph

import scalax.collection.Graph
import scalax.collection.edge.Implicits._

/**
  * Created by sofysmo on 04.12.16.
  */
object ExampleGraph10 {
  val E: Nonterminal = syn ( "(" ~~ E ~~ ")"
    | "N")
  val S = syn(E)
  val g = Graph((0~+#>1)("("),(1~+#>2)("N"),(2~+#>3)(")"),(0~+#>4)("("),(4~+#>2)("N"))

  def main(args: Array[String]): Unit = {
    getResult(S,IGraph(g),"myGraph10")
  }
}
