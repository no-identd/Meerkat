package org.meerkat.parsers

import org.meerkat.util.Input

package object graph {
  def parseGraphAndGetSppfStatistics[T, V](parser: AbstractCPSParsers.AbstractSymbol[T, V], input: Input): Option[SPPFStatistics] =
    parseGraph(parser, input)
      .map { case ParseGraphSuccess(_, _, stat) => stat }
      .toOption
}
