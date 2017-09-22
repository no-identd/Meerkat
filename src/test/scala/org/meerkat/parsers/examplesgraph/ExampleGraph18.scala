package org.meerkat.parsers.examplesgraph

/**
  * Created by sofysmo on 10.12.16.
  */
import org.meerkat.Syntax._
import org.meerkat.parsers.Parsers._
import org.meerkat.util.IGraph
import org.meerkat.parsers._

import scalax.collection.Graph
import scalax.collection.edge.Implicits._

object ExampleGraph18 {
  val E = syn ( "a" ~~ "c" ~~ "d"
    | "a" ~~ "l" ~~"d" )

  val g = Graph((0~+#>1)("a"),(1~+#>2)("c"),(2~+#>3)("d"),(0~+#>4)("a"), (4~+#>2)("l"))
  def main(args: Array[String]): Unit = {
    getResult(E,IGraph(g), "myGraph18")
  }
}
