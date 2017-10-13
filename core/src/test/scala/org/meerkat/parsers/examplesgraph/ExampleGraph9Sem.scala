package org.meerkat.parsers.examplesgraph

import org.meerkat.Syntax._
import org.meerkat.parsers._
import Parsers._
import OperatorParsers._
import org.meerkat.util.{IGraph, InputGraph}

import scalax.collection.Graph
import scalax.collection.edge.Implicits._
/**
  * Created by sofysmo on 28.11.16.
  */
object ExampleGraph9Sem {
  val Int = syn { "[0-9]".r }
  val toInt: String => Int = x => x.toInt

  val E: OperatorNonterminal & Exp =
    syn (  right { E ~~ "^" ~~ E } & { case x~y => Pow(x, y) }
      |> "-" ~~ E               & { Neg(_) }
      |> left ( E ~~ "*" ~~ E    & { case x~y => Mul(x, y) }
      |         E ~~ "/" ~~ E    & { case x~y => Div(x, y) })
      |> left ( E ~~ "+" ~~ E    & { case x~y => Add(x, y) }
      |         E ~~ "-" ~~ E    & { case x~y => Sub(x, y) })
      | "(" ~~ E ~~ ")"
      | Int                    ^ { s => Num(toInt(s)) }
    )
  val g = Graph((0~+#>1)("1"),(1~+#>2)("+"),(2~+#>3)("3"),(3~+#>4)("*"),(4~+#>5)("4"),
                (5~+#>6)("-"),(6~+#>7)("9"),(3~+#>8)("+"),(8~+#>5)("1"))
  def main(args: Array[String]): Unit = {
    val result = execGraph(E($), IGraph(g))//"1+2*3-5")
    println(result)
    //assert(result.isSuccess)
  }

}
