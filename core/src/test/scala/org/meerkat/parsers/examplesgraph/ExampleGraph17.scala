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

object ExampleGraph17 {
  val E = syn ( "a" ~~ "c"
    | "a" )

  val g = Graph((0~+#>1)("a"),(1~+#>2)("c"),(0~+#>1)("a"))
  def main(args: Array[String]): Unit = {
    getResult(E,IGraph(g), "myGraph17")
  }
}
