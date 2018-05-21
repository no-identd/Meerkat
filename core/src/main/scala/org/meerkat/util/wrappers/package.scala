package org.meerkat.util

import org.meerkat.input.Input
import org.meerkat.sppf._
import org.meerkat.tree

import scala.collection.mutable
import scala.util.Try

package object wrappers {
  private def cloneNode(node: SPPFNode, parent: SPPFNode): SPPFNode = {
    val copy = node match {
      case nonterminal @ NonterminalNode(a, b, c) => NonterminalNode(a, b, c)
      case intermediate @ IntermediateNode(a, b, c) => IntermediateNode(a, b, c)
      case terminal @ TerminalNode(a, b, c) => TerminalNode(a, b, c)
      case packed @ PackedNode(a, b) => PackedNode(a, parent.asInstanceOf[NonPackedNode])
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
      case packed: PackedNode => {
        if (packed.leftChild != null) {
          packed.leftChild = constructNodeFromDFSSequence(next.next, packed, next).asInstanceOf[NonPackedNode]
        }
        if (packed.rightChild != null) {
          packed.rightChild = constructNodeFromDFSSequence(next.next, packed, next).asInstanceOf[NonPackedNode]
        }
      }
      case nonpacked: NonPackedNode => {
        if (nonpacked.first != null) {
          nonpacked.first = constructNodeFromDFSSequence(next.next, nonpacked, next).asInstanceOf[PackedNode]
        }
      }
    }

    clone
  }

  def constructTreeFromDFSSequence(sequence: Iterator[SPPFNode]): NonPackedNode = {
    val root = sequence.next.asInstanceOf[NonPackedNode]

    constructNodeFromDFSSequence(root, null, sequence).asInstanceOf[NonPackedNode]
  }

  private def tryToCountTrees(node: SPPFNode, visited: mutable.HashSet[SPPFNode]): Int = {
    if (visited.contains(node))
      throw new Exception();

    visited.add(node);

    node match {
      case packed: PackedNode => packed.children.map(tryToCountTrees(_, visited)).sum - (packed.children.size - 1)
      case terminal: TerminalNode[_] => 1
      case nonpacked: NonPackedNode => nonpacked.children.map(tryToCountTrees(_, visited)).sum
    }
  }

  def tryToCountTrees(root: SPPFNode): Try[Int] = {
    val visited = mutable.HashSet[SPPFNode]();
    Try(tryToCountTrees(root, visited))
  }

  def extractNonAmbiguousSPPFs(roots: Seq[NonPackedNode], converter: SPPFToTreesConverter) = converter(roots)
  def extractNonAmbiguousSPPFs(root: NonPackedNode, converter: SPPFToTreesConverter) = converter(Seq(root))

  def extractTreesFromSPPF(roots: Seq[NonPackedNode], converter: SPPFToTreesConverter)(implicit input: Input[_]):
                    Stream[tree.Tree] =
    converter(roots).map(sppf => TreeBuilder.build(sppf, false))

  def extractTreesFromSPPF(root: NonPackedNode, converter: SPPFToTreesConverter)(implicit input: Input[_]):
                    Stream[tree.Tree] =
    extractTreesFromSPPF(Seq(root), converter)(input)

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
