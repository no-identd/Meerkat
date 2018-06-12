package org.meerkat

import org.meerkat.parsers.Parsers.Nonterminal
import org.scalatest.{FunSuite, Matchers}
import org.meerkat.Syntax._
import org.meerkat.input.LinearInput._
import org.meerkat.parsers.Parsers._
import org.meerkat.parsers._

class LinearInputTest extends FunSuite with Matchers {
  val S: Nonterminal[String, Nothing] =
    syn("a" ~ "b" ~ "a" | "a" ~ S ~ "a")

  implicit val eps: String = ""
  implicit val intEps: Int = 0

  test("simpleTest") {
    getSPPF(S, Vector("a", "a", "b", "a", "a")) shouldBe 'Right
    getSPPF(S, Vector("a", "b", "a", "a", "a")) shouldBe 'Left
  }

  test("predicateTest") {
    val g = new AnyRef {
      val P                            = syn(outE((_: Int) > 0))
      val N                            = syn(outE((_: Int) < 0))
      val S: Nonterminal[Int, Nothing] = syn(P ~ N ~ S | P ~ N)
    }
    getSPPF(g.S, Vector(1, -1, 2, -2)) shouldBe 'Right
    getSPPF(g.S, Vector(1, -1, 2, 3)) shouldBe 'Left
  }
}
