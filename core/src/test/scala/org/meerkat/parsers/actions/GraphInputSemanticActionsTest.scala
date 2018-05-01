package org.meerkat.parsers.actions

import org.meerkat.Syntax._
import org.meerkat.input.GraphxInput
import org.meerkat.parsers.Parsers._
import org.meerkat.parsers._
import org.meerkat.sppf.SemanticAction
import org.scalatest.{FunSuite, Matchers}

import scalax.collection.Graph
import scalax.collection.edge.Implicits._

class GraphInputSemanticActionsTest extends FunSuite with Matchers {
  test("ArgumentTypesTest") {
    val T = syn("a" ^ (a => a))
    val P = syn("+" ^ (p => p))
    val S = syn(
        (T ~ T) & {case t1 ~ t2 => 1}
      | (T ~ "*" ~ T) & {case t1 ~ t2 => 2}
      | (T ~ P ~ T) & {case t1 ~ p ~ t2 => 3})

    val graph = Graph(
      (0 ~+#> 1)("a"),
      (1 ~+#> 2)("a"),
      (1 ~+#> 3)("+"),
      (1 ~+#> 4)("*"),
      (3 ~+#> 5)("a"),
      (4 ~+#> 6)("a")
    )

    implicit val input = GraphxInput(graph)
    val set = getSPPFs(S, input).getOrElse(null)._1.map(sppf => SemanticAction.execute(sppf).asInstanceOf[Int]).toSet
    set shouldBe Set(1, 2, 3)
  }

  private case class Node(value: String, children: List[Node])
  test("ComplicatedOutputTest") {
    val T = syn("a" ^ (a => Node(a, List[Node]())))
    var S: AbstractNonterminal[String, Node] = null
    S = syn(T & (t => t)
         | (S ~ "+" ~ T) & {case l ~ r => Node("+", List(l, r))})

    val graph = Graph(
      (0 ~+#> 1)("a"),
      (1 ~+#> 2)("+"),
      (2 ~+#> 3)("a"),
      (3 ~+#> 4)("+"),
      (4 ~+#> 5)("a")
    )

    implicit val input = GraphxInput(graph)
    val set = getSPPFs(S, input).getOrElse(null)._1.map(sppf => SemanticAction.execute(sppf).asInstanceOf[Node]).toSet
    set.contains(
      Node("+", List(
        Node("+", List(
          Node("a", List()),
          Node("a", List()))),
        Node("a", List())))) shouldBe true
  }

  test("StarAndPlusCombinatorsTest") {
    val A = syn("a" ^ (a => a))
    val T = syn((A+) & (a => a.fold("")({case (z, a) => z.concat(a)})))
    val D = syn((" " ~ T) & (s => s))
    val E = syn((T ~ (D*)) & {case t ~ ts => t +: ts})

    val graph = Graph(
      (0 ~+#> 1)("a"),
      (1 ~+#> 2)("a"),
      (2 ~+#> 3)("a"),
      (2 ~+#> 4)(" "),
      (4 ~+#> 5)("a")
    )

    implicit val input = GraphxInput(graph)
    val set = getSPPFs(E, input).getOrElse(null)._1.map(sppf => SemanticAction.execute(sppf)).toSet

    set shouldBe Set(List("a"), List("aa"), List("aaa"), List("aa", "a"))
  }

  test("PredicatesSupportTest") {
    val N = syn(E((_: String).toInt > 5) ^ (_.toInt))
    val S = syn((N+) & (_.foldRight(1){case (z, v) => z * v}))

    val graph = Graph(
      (0 ~+#> 1)("7"),
      (1 ~+#> 2)("11"),
      (2 ~+#> 3)("13"),
      (0 ~+#> 4)("3"),
      (4 ~+#> 5)("17")
    )

    implicit val input = GraphxInput(graph)
    val set = getSPPFs(S, input).getOrElse(null)._1.map(sppf => SemanticAction.execute(sppf)).toSet

    set shouldBe Set(7, 7*11, 7*11*13)
  }
}
