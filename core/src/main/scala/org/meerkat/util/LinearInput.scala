package org.meerkat.util


class LinearInput[L](list: Vector[L], eps: L) extends Input[L] {
  override type Edge = (L, Int)
  override def length: Int = list.length

  override def outEdges(nodeId: Int): Seq[(L, Int)] =
    if (nodeId >= list.length) Seq.empty
    else Seq((list(nodeId + 1), nodeId + 1))

  override def filterEdges(id: Int, label: L): scala.Seq[Int] =
    if (id >= list.length) Seq.empty
    else if (list(id) == label) Seq(id + 1)
    else Seq.empty

  override def checkNode(id: Int, label: L): Boolean = true

  // TODO: get rid of
  override def substring(start: Int, end: Int): String = ???

  // TODO: get rid of
  override def epsilonLabel: Any = eps
}

object LinearInput {
  implicit def toInput[L](list: Vector[L])(implicit eps: L): LinearInput[L] =
    new LinearInput(list, eps)
}
