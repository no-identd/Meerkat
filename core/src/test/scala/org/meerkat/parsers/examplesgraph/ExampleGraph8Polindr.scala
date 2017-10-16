package org.meerkat.parsers.examplesgraph

import org.meerkat.Syntax._
import org.meerkat.parsers.Parsers._
import org.meerkat.parsers._


/**
  * Created by sofysmo on 28.11.16.
  */
object ExampleGraph8Polindr {
  val E: Nonterminal = syn ( "a" ~~ E ~~ "a"
    | "b" ~~ E ~~ "b" | "c")
  val g = "abacaba"
  //val g = Graph((0~+>1)("("),(1~+>2)("N"),(2~+>3)(")"))

  def main(args: Array[String]): Unit = {
    getResult(E,g,"SPPFPolindrom")
  }
}
