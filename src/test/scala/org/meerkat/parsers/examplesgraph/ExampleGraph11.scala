package org.meerkat.parsers.examplesgraph

import org.meerkat.Syntax._
import org.meerkat.parsers.Parsers._
import org.meerkat.parsers._
import org.meerkat.util.IGraph

import scalax.collection.Graph
import scalax.collection.edge.Implicits._

/**
  * Created by sofysmo on 06.12.16.
  */
object ExampleGraph11 {
  val S: Nonterminal =
    syn ( "a" ~~ S ~~ "b"
      |"a" ~~  S
      | "c"
    )
  val g = Graph((0~+#>1)("a"),(1~+#>2)("a"), (1~+#>2)("c"), (2~+#>3)("b"))
  def main(args: Array[String]): Unit = {
    getResult(S,IGraph(g),"myGraph5")
  }
}

