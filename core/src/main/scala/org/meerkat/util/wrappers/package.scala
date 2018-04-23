package org.meerkat.util

import org.meerkat.tree.{RuleNode, TerminalNode, Tree}

package object wrappers {
  private def extractPath(root: Tree, isMostLeft: Boolean): Seq[Int] = root match {
    case node @ RuleNode(_, children) => extractPath(children.head, isMostLeft) ++
      children.tail.flatMap(child => extractPath(child, false))

    case node @ TerminalNode(_, leftExtent, rightExtent) =>
      if (isMostLeft) Seq(leftExtent, rightExtent) else Seq(rightExtent)
  }

  def extractPath(root: Tree): Seq[Int] = root match {
    case node @ RuleNode(_, children) => extractPath(children.head, true) ++
      children.tail.flatMap(child => extractPath(child, false))

    case node @ TerminalNode(_, leftExtent, rightExtent) =>
      Seq(leftExtent, rightExtent)
  }
}
