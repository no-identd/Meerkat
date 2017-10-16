package org.meerkat.parsers.examplesgraph


/**
  * Created by sofysmo on 06.12.16.
  */
import org.meerkat.Syntax._
import org.meerkat.parsers.Parsers._
import org.meerkat.util.IGraph
import org.meerkat.parsers._

import scalax.collection.Graph
import scalax.collection.edge.Implicits._

object ExampleGraph15 {
  val E = syn ( "a"
    | "b" )

  val g = Graph((0~+#>1)("a"),(0~+#>1)("b"))
  def main(args: Array[String]): Unit = {
    getResult(E,IGraph(g), "myGraph15")
  }
}
/*import org.meerkat.Syntax._
import org.meerkat.parsers._
import org.meerkat.parsers.Parsers._
import org.meerkat.util.IGraph


import scalax.collection.Graph
import scalax.collection.edge.Implicits.

/**
  * Created by sofysmo on 06.12.16.
  */
object ExampleGraph15 {
  val E = syn ( "a"
    | "b" )
  val g = Graph((0~+>1)("a"),(0~+>1)("b"))
  def main(args: Array[String]): Unit = {
    getResult(E,IGraph(g), "myGraph14")
  }
}*/
