package org.meerkat.util.converters

import org.meerkat.Syntax._
import org.meerkat.input.GraphxInput
import org.meerkat.parsers.Parsers._
import org.meerkat.parsers._
import org.meerkat.graph.parseGraphFromAllPositions
import org.scalatest.{FunSuite, Matchers}
import scalax.collection.Graph
import scalax.collection.edge.Implicits._

class ComplicatedGrammarTest extends FunSuite with Matchers {
  val eps = syn("e")
  val S1 = syn("exit36" | "-entry36" | eps)
  val SS1 = syn("entry36" | "-exit36" | eps)
  val road = syn("name" | "data" | "arr_elem"| eps)
  val neroad = syn("-name" | "-data" | "-arr_elem"| eps | epsilon)
  val C: Nonterminal[String, String] = syn(SS1 ~ C ~ S1 | C ~ C | neroad | road | eps | epsilon)
  val memAlias: Nonterminal[String, String] = syn(road ~ memAlias ~ neroad | memAlias ~ memAlias | C | eps)

  val g = Graph(
    (0 ~+#> 1) ("e"),
    (2 ~+#> 3) ("e"),
    (4 ~+#> 5) ("e"),
    (6 ~+#> 7) ("e"),
    (10 ~+#> 8) ("e"),
    (9 ~+#> 11) ("e"),
    (1 ~+#> 3) ("name"),
    (3 ~+#> 1) ("-name"),
    (3 ~+#> 5) ("e"),
    (8 ~+#> 7) ("name"),
    (7 ~+#> 8) ("-name"),

    (12 ~+#> 15) ("e"),
    (13 ~+#> 16) ("e"),
    (14 ~+#> 17) ("e"),

    (11 ~+#> 15) ("entry36"),
    (16 ~+#> 8) ("-exit36"),
    (15 ~+#> 11) ("-entry36"),
    (8 ~+#> 16) ("exit36"),

    (15 ~+#> 16) ("data"),
    (17 ~+#> 16) ("-arr_elem"),
    (16 ~+#> 15) ("-data"),
    (16 ~+#> 17) ("arr_elem"),
  )

  test("ComplicatedGrammarTest") {
    implicit val input = GraphxInput(g)
    var sppf = parseGraphFromAllPositions(memAlias, GraphxInput(g))

    sppf.foreach(root => {
      extractNonAmbiguousSPPFs(root)
        .slice(100, 105)
        .foreach(s => s.leftExtent shouldBe root.leftExtent)
    })
  }
}
