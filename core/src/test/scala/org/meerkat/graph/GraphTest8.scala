package org.meerkat.graph

import org.meerkat.Syntax._
import org.meerkat.parsers.Parsers._
import org.meerkat.parsers._
import org.meerkat.util.IGraph
import org.scalatest.FunSuite
import org.scalatest.Matchers._
import org.scalatest.OptionValues._

import scalax.collection.Graph
import scalax.collection.edge.Implicits._

class GraphTest8 extends FunSuite {
  val E: Nonterminal = syn(
    "(" ~~ E ~~ ")" |
      "N")
  val S = syn(E)
  val g = Graph(
    (0 ~+#> 1) ("("),
    (1 ~+#> 2) ("N"),
    (2 ~+#> 3) (")"),
    (0 ~+#> 4) ("("),
    (4 ~+#> 2) ("N")
  )

  test("sppfStat") {
    parseGraphAndGetSppfStatistics(S, IGraph(g)).value shouldBe SPPFStatistics(4, 2, 5, 7, 1)
  }
}
