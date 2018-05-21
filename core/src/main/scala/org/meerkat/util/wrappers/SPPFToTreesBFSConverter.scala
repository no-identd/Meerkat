package org.meerkat.util.wrappers

import org.meerkat.input.Input
import org.meerkat.sppf.NonPackedNode
import org.meerkat.sppf._

import scala.collection.mutable

private case class Context(var nodes: Stream[SPPFNode], val queue: mutable.Queue[SPPFNode])

/*
object SPPFToTreesBFSConverter extends SPPFToTreesConverter {
  def apply(roots: Seq[NonPackedNode]) =
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

}
*/