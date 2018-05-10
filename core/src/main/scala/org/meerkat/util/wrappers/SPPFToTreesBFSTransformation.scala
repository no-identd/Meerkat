package org.meerkat.util.wrappers

import org.meerkat.input.Input
import org.meerkat.sppf.NonPackedNode
import org.meerkat.sppf._

import scala.collection.mutable

private case class Context(var nodes: Stream[SPPFNode], val queue: mutable.Queue[SPPFNode])

object SPPFToTreesBFSTransformation {
  def extractNonAmbiguousSPPFs(roots: Seq[NonPackedNode]) =
    Stream.iterate(
      (Seq[NonPackedNode](), roots.map(root => Context(Stream(), mutable.Queue(root)))) // Initial state
      ) ({case (_, contextSeq) =>                             // For current state
          contextSeq
          .map(contextStep)                                   // Make step for each context
          .foldLeft((Seq[NonPackedNode](), Seq[Context]())) { // Collect results
            case ((readySeq, newContexts), (ready, currentContexts)) =>
              (if (ready != null) readySeq :+ ready else readySeq, // If there is ready-to-extract context then collect it
               newContexts ++ currentContexts)                     // Concat all produced contexts
        }})
      .takeWhile({case (ready, contextSeq) => !contextSeq.isEmpty || !ready.isEmpty}) // Iterate until all contexts are ready
      .flatMap({case (ready, _) => ready})                                            // Collect all produced trees

  def extractNonAmbiguousSPPFs(root: NonPackedNode): Stream[NonPackedNode] = extractNonAmbiguousSPPFs(Seq(root))

  def extractTreesFromSPPF(sppf: Seq[NonPackedNode])(implicit input: Input[_, _]) =
    extractNonAmbiguousSPPFs(sppf).map(root => TreeBuilder.build(root, false)(input))

  def extractTreesFromSPPF(sppf: NonPackedNode)(implicit input: Input[_, _]) =
    extractNonAmbiguousSPPFs(sppf).map(root => TreeBuilder.build(root, false)(input))

  private def contextStep(context: Context): (NonPackedNode, Seq[Context]) = {
    if (context.queue.isEmpty)
      (constructTree(context.nodes.reverse), Seq())
    else {
      val node = context.queue.dequeue()
      context.nodes = node +: context.nodes

      if (node.isInstanceOf[NonPackedNode]) {
        val nonpacked = node.asInstanceOf[NonPackedNode]

        val rest = if (nonpacked.rest == null) Seq() else
          nonpacked.rest.map(way => {
            val queue = context.queue.clone()
            queue.enqueue(way)
            Context(context.nodes, queue)
          })

        if (nonpacked.first != null)
          context.queue.enqueue(nonpacked.first)

        (null, Seq(context) ++ rest)
      } else {
        context.queue.enqueue(node.children: _*)

        (null, Seq(context))
      }
    }
  }

  private def constructTree(nodes: Stream[SPPFNode]): NonPackedNode = {
    val queue = mutable.Queue(nodes: _*)
    val root = cloneNode(queue.dequeue(), null).asInstanceOf[NonPackedNode]

    var currentLevel = Seq[SPPFNode](root)
    while (!queue.isEmpty) {
      currentLevel = currentLevel.flatMap(node => fillNode(node, queue))
    }

    root
  }

  private def fillNode(node: SPPFNode, queue: mutable.Queue[SPPFNode]): Seq[SPPFNode] = {
    if (node.isInstanceOf[NonPackedNode]) {
      val nonpacked = node.asInstanceOf[NonPackedNode]

      if (nonpacked.first != null) {
        nonpacked.first = cloneNode(queue.dequeue(), nonpacked).asInstanceOf[PackedNode]
        Seq(nonpacked.first)
      } else Seq()
    } else {
      val packed = node.asInstanceOf[PackedNode]

      if (packed.leftChild != null)
        packed.leftChild = cloneNode(queue.dequeue(), packed).asInstanceOf[NonPackedNode]
      if (packed.rightChild != null)
        packed.rightChild = cloneNode(queue.dequeue(), packed).asInstanceOf[NonPackedNode]

      packed.children
    }
  }

  private def cloneNode(node: SPPFNode, parent: SPPFNode): SPPFNode = {
    val copy = node match {
      case _ @ NonterminalNode(a, b, c) => NonterminalNode(a, b, c)
      case _ @ IntermediateNode(a, b, c) => IntermediateNode(a, b, c)
      case _ @ TerminalNode(a, b, c) => TerminalNode(a, b, c)
      case _ @ EpsilonNode(a) => EpsilonNode(a)
      case packed @ PackedNode(a, b) => PackedNode(a, parent.asInstanceOf[NonPackedNode])
    }

    if (copy.isInstanceOf[NonPackedNode]) {
      val nonpackedNode = node.asInstanceOf[NonPackedNode]
      val nonpackedCopy = copy.asInstanceOf[NonPackedNode]
      nonpackedCopy.first = nonpackedNode.first
    } else {
      val packedNode = node.asInstanceOf[PackedNode]
      val packedCopy = copy.asInstanceOf[PackedNode]
      packedCopy.leftChild = packedNode.leftChild
      packedCopy.rightChild = packedNode.rightChild
    }

    copy
  }
}
