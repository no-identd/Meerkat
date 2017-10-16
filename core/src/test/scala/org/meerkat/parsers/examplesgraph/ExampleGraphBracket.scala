package org.meerkat.parsers.examplesgraph

import org.meerkat.parsers.Parsers.SequenceBuilder
import org.meerkat.parsers.~

import org.meerkat.Syntax._
import org.meerkat.parsers._
import org.meerkat.parsers.Parsers._
import org.meerkat.util.IGraph

import scalax.collection.Graph
import scalax.collection.edge.Implicits._

/**
 * Created by sofysmo on 25.12.16.
 */
object ExampleGraphBracket {
        val toStr: String => String = x => x
        val S: Nonterminal  = syn ( "(" ~~ S ~~ ")"~~ S | epsilon )
        val g = Graph((0~+#>1)('('), (0~+#>3)(')'),
        (1~+#>2)(')'), (2~+#>8)('('), (8~+#>9)(')'), (3~+#>4)('('), (4~+#>7)(')'),
        (0~+#>5)('('),(5~+#>6)(')'))

        def main(args: Array[String]): Unit = {
            getResult(S, IGraph(g), "myGraphBracket")
        }
}
