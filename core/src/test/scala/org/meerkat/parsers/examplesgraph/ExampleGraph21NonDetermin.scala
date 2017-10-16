package org.meerkat.parsers.examplesgraph

/**
  * Created by sofysmo on 13.12.16.
  */
import org.meerkat.Syntax._
import org.meerkat.parsers.Parsers._
import org.meerkat.util.IGraph
import org.meerkat.parsers._

import scalax.collection.Graph
import scalax.collection.edge.Implicits._

object ExampleGraph21NonDetermin {
  val E = syn ( "a" ~~ "c" ~~ "d"
    | "b" ~~ "c")

  val g = Graph((0~+#>1)("a"),(0~+#>3)("a"),(0~+#>5)("a"), (5~+#>6)("c"), (6~+#>7)("d"), (1~+#>2)("c"), (3~+#>2)("c"), (2~+#>4)("d"))
  def main(args: Array[String]): Unit = {
    getResult(E,IGraph(g), "myGraph21")
  }
}
