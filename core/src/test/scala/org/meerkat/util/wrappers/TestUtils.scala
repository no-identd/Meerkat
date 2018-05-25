package org.meerkat.util.wrappers

import org.meerkat.tree.{RuleNode, TerminalNode, Tree}

object TestUtils {
  def terminalNode[E](value: E) = TerminalNode(value, -1, -1)

  def printTreeWithoutExtents(root: Tree): String = root match {
    case node @ RuleNode(rule, children) => rule.toString + "[" +
      children.foldLeft("")({case (string, tree) => string + printTreeWithoutExtents(tree)}) + "]"
    case node @ TerminalNode(value, _, _) => value.toString
  }

  def compareTreesIgnoringExtents(first: Tree, second: Tree): Boolean =
    printTreeWithoutExtents(first).equals(printTreeWithoutExtents(second))

  def treeSize(root: Tree): Int = root match {
    case root @ RuleNode(_, children) => children.foldRight(1)((root, value) => value + treeSize(root))
    case node @ TerminalNode(_, _, _) => 1
  }
}
