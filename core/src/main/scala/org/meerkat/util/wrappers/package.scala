package org.meerkat.util

import org.meerkat.input.Input
import org.meerkat.sppf._
import org.meerkat.tree

import scala.collection.mutable
import scala.util.Try

package object wrappers {
  private def cloneNode(node: SPPFNode, parent: SPPFNode): SPPFNode = {
    val copy = node match {
      case nonterminal @ NonterminalNode(name, le, re) => NonterminalNode(name, le, re)
      case intermediate @ IntermediateNode(name, le, re) => IntermediateNode(name, le, re)
      case packed @ PackedNode(slot, _) => PackedNode(slot, parent.asInstanceOf[NonPackedNode])
      case terminal @ TerminalNode(s, le, re) => TerminalNode(s, le, re)
      case epsilon @ EpsilonNode(e) => EpsilonNode(e)
      case vertex @ VertexNode(s, e) => VertexNode(s, e)
    }

    copy match {
      case nonpackedCopy: NonPackedNode =>
        val nonpackedNode = node.asInstanceOf[NonPackedNode]
        nonpackedCopy.first = nonpackedNode.first
      case _ =>
        val packedNode = node.asInstanceOf[PackedNode]
        val packedCopy = copy.asInstanceOf[PackedNode]
        packedCopy.leftChild = packedNode.leftChild
        packedCopy.rightChild = packedNode.rightChild
    }

    copy.asInstanceOf[SPPFNode]
  }

  private def constructNodeFromDFSSequence(current: SPPFNode, parent: SPPFNode, next: Iterator[SPPFNode]): SPPFNode = {
    val clone = cloneNode(current, parent)

    clone match {
      case packed: PackedNode =>
        if (packed.leftChild != null) {
          packed.leftChild = constructNodeFromDFSSequence(next.next, packed, next).asInstanceOf[NonPackedNode]
        }
        if (packed.rightChild != null) {
          packed.rightChild = constructNodeFromDFSSequence(next.next, packed, next).asInstanceOf[NonPackedNode]
        }
      case nonpacked: NonPackedNode =>
        if (nonpacked.first != null) {
          nonpacked.first = constructNodeFromDFSSequence(next.next, nonpacked, next).asInstanceOf[PackedNode]
        }
    }

    clone
  }

  def constructTreeFromDFSSequence(sequence: Iterator[SPPFNode]): NonPackedNode = {
    val root = sequence.next.asInstanceOf[NonPackedNode]

    constructNodeFromDFSSequence(root, null, sequence).asInstanceOf[NonPackedNode]
  }

  private def fillNode(node: SPPFNode, sequence: Iterator[SPPFNode]): Seq[SPPFNode] = {
    node match {
      case nonpacked: NonPackedNode =>
        if (nonpacked.first != null) {
          nonpacked.first = cloneNode(sequence.next, nonpacked).asInstanceOf[PackedNode]
          Seq(nonpacked.first)
        } else Seq()
      case _ =>
        val packed = node.asInstanceOf[PackedNode]

        if (packed.leftChild != null)
          packed.leftChild = cloneNode(sequence.next, packed).asInstanceOf[NonPackedNode]
        if (packed.rightChild != null)
          packed.rightChild = cloneNode(sequence.next, packed).asInstanceOf[NonPackedNode]

        packed.children
    }
  }

  def constructTreeFromBFSSequence(sequence: Iterator[SPPFNode]): NonPackedNode = {
    val root = cloneNode(sequence.next, null).asInstanceOf[NonPackedNode]

    var currentLevel = Seq[SPPFNode](root)
    while (sequence.hasNext) {
      currentLevel = currentLevel.flatMap(node => fillNode(node, sequence))
    }

    root
  }

  private class CyclicSPPFException extends Exception;

  private def tryToCountTrees(node: SPPFNode, visited: mutable.Set[SPPFNode]): Int = {
    if (visited.contains(node))
      throw new CyclicSPPFException()

    visited.add(node)

    val count = node match {
      case packed: PackedNode => packed.children.map(tryToCountTrees(_, visited)).sum - (packed.children.size - 1)
      case terminal: TerminalNode[_] => 1
      case nonpacked: NonPackedNode => nonpacked.children.map(tryToCountTrees(_, visited)).sum
    }

    visited.remove(node)

    count
  }

  def tryToCountTrees(root: SPPFNode): Try[Int] = {
    val visited = mutable.HashSet[SPPFNode]()
    Try(tryToCountTrees(root, visited))
  }

  def findAllCycles(roots: Seq[SPPFNode]): Set[(SPPFNode, SPPFNode)] = {
    val visited = mutable.HashSet[SPPFNode]()
    val cycles = mutable.HashSet[(SPPFNode, SPPFNode)]()

    var findCycles: SPPFNode => Unit = null
    findCycles = (node: SPPFNode) => {
      visited.add(node)

      node.children.foreach(child => {
        if (visited.contains(child))
          cycles.add((node, child))
        else
          findCycles(child)
      })

      visited.remove(node)
    }

    roots.foreach(findCycles)
    cycles.toSet
  }

  def extractNonAmbiguousSPPFs(roots: Seq[NonPackedNode],
                               converter: SPPFToTreesConverter = SPPFToTreesEnumeratingConverter) = converter(roots)

  def extractNonAmbiguousSPPFs(root: NonPackedNode, converter: SPPFToTreesConverter): Stream[NonPackedNode] =
    extractNonAmbiguousSPPFs(Seq(root), converter)
  def extractNonAmbiguousSPPFs(root: NonPackedNode): Stream[NonPackedNode] =
    extractNonAmbiguousSPPFs(Seq(root))

  def extractTreesFromSPPF(roots: Seq[NonPackedNode],
                           converter: SPPFToTreesConverter = SPPFToTreesEnumeratingConverter)
                          (implicit input: Input[_, _]): Stream[tree.Tree] =
    converter(roots).map(sppf => TreeBuilder.build(sppf, false))

  def extractTreesFromSPPF(root: NonPackedNode, converter: SPPFToTreesConverter)
                          (implicit input: Input[_, _]): Stream[tree.Tree] =
    extractTreesFromSPPF(Seq(root), converter)(input)

  def extractTreesFromSPPF(root: NonPackedNode)
                          (implicit input: Input[_, _]): Stream[tree.Tree] =
    extractTreesFromSPPF(Seq(root))(input)

  private def extractPath(root: tree.Tree, isMostLeft: Boolean): Seq[Int] = root match {
    case node @ tree.RuleNode(_, children) => extractPath(children.head, isMostLeft) ++
      children.tail.flatMap(child => extractPath(child, false))

    case node @ tree.TerminalNode(_, leftExtent, rightExtent) =>
      if (isMostLeft) Seq(leftExtent, rightExtent) else Seq(rightExtent)
  }

  def extractPath(root: tree.Tree): Seq[Int] = root match {
    case node @ tree.RuleNode(_, children) => extractPath(children.head, true) ++
      children.tail.flatMap(child => extractPath(child, false))

    case node @ tree.TerminalNode(_, leftExtent, rightExtent) =>
      Seq(leftExtent, rightExtent)
  }
}
