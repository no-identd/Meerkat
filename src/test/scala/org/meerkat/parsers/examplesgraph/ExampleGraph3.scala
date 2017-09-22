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
object ExampleGraph3 {
  val E: Nonterminal
  = syn ( E ~~ "+" ~~ E// & { case x~y => x+y}
    | E ~~ "*" ~~ E //& { case x~y => x*y}
    | Num )//^ {toInt} )

  val Num = syn("1"|"0")

  val g = Graph((0~+#>1)("0"),(0~+#>2)("+"),
    (1~+#>2)("*"),(2~+#>0)("1"))
  def main(args: Array[String]): Unit = {
    getResult(E,IGraph(g),"myGraph3")
  }

}
