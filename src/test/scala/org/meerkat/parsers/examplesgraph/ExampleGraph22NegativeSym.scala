package org.meerkat.parsers.examplesgraph

/**
  * Created by sofysmo on 03.05.17.
  */

import org.meerkat.Syntax._
import org.meerkat.parsers.Parsers._
import org.meerkat.util.IGraph
import org.meerkat.parsers._

import scalax.collection.Graph
import scalax.collection.edge.Implicits._

object ExampleGraph22NegativeSym {
  val E = syn{ not{"a"} ~~ not{"b"}}

  val g = Graph((2~+#>1)("a"),(2~+#>1)("b"),(1~+#>0)("a"))
  def main(args: Array[String]): Unit = {
    getResult(E,IGraph(g), "myGraph22")
  }
}
