package org.meerkat.parsers.graph

import org.meerkat.Syntax._
import org.meerkat.parsers.Parsers._
import org.meerkat.parsers._
import org.meerkat.util.IGraph
import org.scalatest.FunSuite
import org.scalatest.Matchers._
import org.scalatest.OptionValues._

import scalax.collection.Graph
import scalax.collection.edge.Implicits._

class GraphTest13 extends FunSuite {
  val S: Nonterminal = syn(
    "(" ~~ S ~~ ")" ~~ S |
      epsilon
  )
  val g = Graph(
    (0 ~+#> 1) ('('),
    (0 ~+#> 3) (')'),
    (1 ~+#> 2) (')'),
    (2 ~+#> 8) ('('),
    (8 ~+#> 9) (')'),
    (3 ~+#> 4) ('('),
    (4 ~+#> 7) (')'),
    (0 ~+#> 5) ('('),
    (5 ~+#> 6) (')')
  )

  test("sppfStat") {
    parseGraphAndGetSppfStatistics(S, IGraph(g)).value shouldBe SPPFStatistics(11, 10, 13, 21, 0)
  }
}
