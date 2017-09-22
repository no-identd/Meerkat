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
object ExampleGraph4 {
  val E: Nonterminal = syn ( "(" ~~ E ~~ ")"
    | epsilon)
  val g = Graph((0~+#>1)("("),(1~+#>2)("("),
    (2~+#>0)("("),(0~+#>3)(")"), (3~+#>0)(")"))

  def main(args: Array[String]): Unit = {
    getResult(E, IGraph(g), "myGraph4")
  }
}
