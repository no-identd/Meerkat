package org.meerkat.graph
import org.meerkat.parsers.CPSResult
import org.meerkat.parsers.Parsers._
import org.meerkat.sppf.{NonPackedNode, SPPFLookup, TerminalNode}
import org.meerkat.tree.{TerminalSymbol, VertexSymbol}
import org.meerkat.util.Input

object GraphParsers {
  def E(label: String): Terminal =
    terminal(label)

  // TODO: fix naming if critical
  def anyE[Edge]: Terminal = new Terminal {
    def apply(input: Input, i: Int, sppfLookup: SPPFLookup): CPSResult[TerminalNode] =
      input.outEdges(i) match {
        case edges if edges.isEmpty => CPSResult.failure
        case edges =>
          val terminals = edges.map {
            case (edgeName, to) =>
              CPSResult.success(sppfLookup.getTerminalNode(edgeName, i, to))
          }
          terminals.reduceLeft(_.orElse(_))
      }
    override def name: String     = "anyE"
    override def symbol           = TerminalSymbol(name)
    override def toString: String = name
  }

  def V[Node](label: String): Vertex = new Vertex {
    def apply(input: Input, i: Int, sppfLookup: SPPFLookup): CPSResult[NonPackedNode] =
      if (input.checkNode(i, label))
        CPSResult.success(sppfLookup.getEpsilonNode(i))
      else CPSResult.failure

    override def symbol: VertexSymbol = VertexSymbol("label")
    override def name: String         = "label"
  }

}
