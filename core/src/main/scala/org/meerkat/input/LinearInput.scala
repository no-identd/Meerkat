package org.meerkat.input

import scala.language.implicitConversions

class LinearInput[L](list: Vector[L]) extends Input[L, Nothing] {
  override def nodesCount: Int = list.length

  override def filterEdges(nodeId: Int,
                           predicate: L => Boolean,
                           outgoing: Boolean): Seq[(L, Int)] =
    if (outgoing) {
      if (nodeId >= list.length) Seq.empty
      else if (predicate(list(nodeId))) Seq((list(nodeId), nodeId + 1))
      else Seq.empty
    } else Seq.empty

  override def checkNode(nodeId: Int,
                         predicate: Nothing => Boolean): Option[Nothing] = None
}

object LinearInput {
  implicit def toInput[L, N](vec: Vector[L]): LinearInput[L] =
    new LinearInput(vec)
}
