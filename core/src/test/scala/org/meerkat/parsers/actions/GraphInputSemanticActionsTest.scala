package org.meerkat.parsers.actions

import org.meerkat.Syntax._
import org.meerkat.input.{GraphxInput, Input}
import org.meerkat.parsers.Parsers._
import org.meerkat.parsers._
import org.meerkat.sppf.SemanticAction
import org.meerkat.graph.parseGraphFromAllPositions
import org.scalatest.{FunSuite, Matchers}
import scalax.collection.Graph
import scalax.collection.edge.Implicits._
import scalax.collection.edge.LkDiEdge

import scala.language.postfixOps

class GraphInputSemanticActionsTest extends FunSuite with Matchers {
  test("ArgumentTypesTest") {
    val T = syn("a" ^ (a => a))
    val P = syn("+" ^ (p => p))
    val S = syn((T ~ T) & { case t1 ~ t2 => 1 }
      | (T ~ "*" ~ T) & { case t1 ~ t2   => 2 }
      | (T ~ P ~ T) & { case t1 ~ p ~ t2 => 3 })

    val graph = Graph(
      (0 ~+#> 1)("a"),
      (1 ~+#> 2)("a"),
      (1 ~+#> 3)("+"),
      (1 ~+#> 4)("*"),
      (3 ~+#> 5)("a"),
      (4 ~+#> 6)("a")
    )

    implicit val input = GraphxInput(graph)
    val set = getSPPFs(S, input)
      .getOrElse(null)
      ._1
      .map(sppf => SemanticAction.execute(sppf).asInstanceOf[Int])
      .toSet
    set shouldBe Set(1, 2, 3)
  }

  private case class Node(value: String, children: List[Node])
  test("ComplicatedOutputTest") {
    val T                                             = syn("a" ^ (a => Node(a, List[Node]())))
    var S: AbstractNonterminal[String, Nothing, Node] = null
    S = syn(
      T & (t => t)
        | (S ~ "+" ~ T) & { case l ~ r => Node("+", List(l, r)) })

    val graph = Graph(
      (0 ~+#> 1)("a"),
      (1 ~+#> 2)("+"),
      (2 ~+#> 3)("a"),
      (3 ~+#> 4)("+"),
      (4 ~+#> 5)("a")
    )

    implicit val input = GraphxInput(graph)
    val set = getSPPFs(S, input)
      .getOrElse(null)
      ._1
      .map(sppf => SemanticAction.execute(sppf).asInstanceOf[Node])
      .toSet
    set.contains(
      Node("+",
           List(Node("+", List(Node("a", List()), Node("a", List()))),
                Node("a", List())))) shouldBe true
  }

  test("StarAndPlusCombinatorsTest") {
    val A = syn("a" ^ (a => a))
    val T = syn((A +) & (a => a.fold("")({ case (z, a) => z.concat(a) })))
    val D = syn((" " ~ T) & (s => s))
    val E = syn((T ~ (D *)) & { case t ~ ts => t +: ts })

    val graph = Graph(
      (0 ~+#> 1)("a"),
      (1 ~+#> 2)("a"),
      (2 ~+#> 3)("a"),
      (2 ~+#> 4)(" "),
      (4 ~+#> 5)("a")
    )

    implicit val input = GraphxInput(graph)
    val set = getSPPFs(E, input)
      .getOrElse(null)
      ._1
      .map(sppf => SemanticAction.execute(sppf))
      .toSet

    set shouldBe Set(List("a"), List("aa"), List("aaa"), List("aa", "a"))
  }

  test("EdgePredicatesSupportTest") {
    val N = syn(outE((_: String).toInt > 5) ^ (_.toInt))
    val S = syn((N +) & (_.foldRight(1) { case (z, v) => z * v }))

    val graph = Graph(
      (0 ~+#> 1)("7"),
      (1 ~+#> 2)("11"),
      (2 ~+#> 3)("13"),
      (0 ~+#> 4)("3"),
      (4 ~+#> 5)("17")
    )

    implicit val input = GraphxInput(graph)
    val set = getSPPFs(S, input)
      .getOrElse(null)
      ._1
      .map(sppf => SemanticAction.execute(sppf))
      .toSet

    set shouldBe Set(7, 7 * 11, 7 * 11 * 13)
  }

  private class MyGraphxInput[L](graph: Graph[Int, LkDiEdge])
      extends Input[L, Int] {

    override def nodesCount: Int = graph.order

    override def filterEdges(nodeId: Int,
                             predicate: L => Boolean,
                             outgoing: Boolean): collection.Seq[(L, Int)] = {
      val edges =
        if (outgoing) graph.get(nodeId).outgoing
        else graph.get(nodeId).incoming
      edges.collect {
        case e if predicate(e.label.asInstanceOf[L]) =>
          (e.label.asInstanceOf[L], e.to.value)
      }.toSeq
    }

    override def checkNode(nodeId: Int,
                           predicate: Int => Boolean): Option[Int] =
      if (predicate(nodeId)) Option(nodeId) else Option.empty
  }

  test("VertexPredicatesSupportTest") {
    val N = syn(V((_: Int) >= 2) ^ (n => n))
    val D = syn(outE((_: String) => true))
    val S = syn(N ~ D ~ N & { case a ~ b => a + b })

    val graph = Graph(
      (0 ~+#> 1)("a"),
      (2 ~+#> 3)("b")
    )

    implicit val input = new MyGraphxInput(graph)
    val set = parseGraphFromAllPositions(S, input)
      .map(sppf => SemanticAction.execute(sppf))
      .toSet

    set shouldBe Set(5)
  }
}
