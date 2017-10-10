/*package org.meerkat.parsers.examplesgraph

import org.meerkat.Syntax._
import org.meerkat.parsers.Parsers._
import org.meerkat.parsers._
import org.meerkat.sppf.SemanticAction
import org.meerkat.util.IGraph

import scalax.collection.Graph
import scalax.collection.edge.Implicits._

/**
  * Created by sofysmo on 27.11.16.
  */
object ExampleGraph1 {
  val A = syn { "a" }
  val B = syn { "b" }
  val AB: SequenceBuilder[String~String] = A ~~ B

  val S =
    syn ( A ~~ A ~~ B
      | "c"
    )
  val g = Graph((0~+#>1)('a'), (1~+#>2)('a'), (1~+#>2)('c'), (2~+#>3)('b'))
  def main(args: Array[String]): Unit = {
    val res = getSPPFs(S,IGraph(g))
    res match {
      case Left(_) => println("Can't make Sppf")
      case Right((roots,_,_)) =>{
        //val x = SemanticAction.execute(roots.head)(IGraph(g)).asInstanceOf[String]
        //roots.foreach(root => Visualizer.visualize(Visualizer.toDot(root, input),filename+root,"."))
      }
    }
    //getResult(S,g,"myGraph1")
  }
}*/
