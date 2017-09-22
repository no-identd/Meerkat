package org.meerkat.parsers.examples

import org.meerkat.Syntax._
import org.meerkat.parsers._
import org.junit.runner.RunWith
import org.meerkat.parsers.Parsers._
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

@RunWith(classOf[JUnitRunner])
class Example16 extends FunSuite {

  //val A = syn { "a" ^ toStr }
  //val B = syn { "b" ^ toStr }

  //val AB: SequenceBuilder[String~String] = A ~ B

  val S: Nonterminal =
    syn ( "a" ~~ S ~~ "b"
      |"a" ~~  S
      | "c"
    )


  test("test") {
    val result = exec(S, "ac")

    //assert(result.isSuccess)
    //assert(result.asSuccess == "a++b")
  }

}
