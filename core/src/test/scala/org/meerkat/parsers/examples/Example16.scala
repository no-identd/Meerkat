package org.meerkat.parsers.examples

import org.meerkat.Syntax._
import org.meerkat.parsers._
import org.junit.runner.RunWith
import org.meerkat.parsers.Parsers._
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.meerkat.util.LinearInput._


@RunWith(classOf[JUnitRunner])
class Example16 extends FunSuite {

  val S: Nonterminal[Char] =
    syn(
      "a" ~ S ~ "b"
        | "c"
    )

  test("test") {
    val result = parse(S, "acb")
    assert(result.isSuccess)
  }

}
