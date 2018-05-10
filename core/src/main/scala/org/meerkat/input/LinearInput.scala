package org.meerkat.input

class LinearInput[L](list: Vector[L], eps: L) extends Input[L] {
  override type M = L
  override def edgesCount: Int = list.length

  // TODO: get rid of
  override def epsilonLabel: Any = eps

  override def filterEdges(nodeId: Int, predicate: L => Boolean): Seq[(L, Int)] =
    if (nodeId >= list.length) Seq.empty
    else if (predicate(list(nodeId))) Seq((list(nodeId), nodeId + 1))
    else Seq.empty


  override def checkNode(nodeId: Int, predicate: L => Boolean): Boolean = true
}

object LinearInput {
  implicit def toInput[L](vec: Vector[L])(implicit eps: L): LinearInput[L] =
    new LinearInput(vec, eps)
}
