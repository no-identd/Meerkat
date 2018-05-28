package org.meerkat.util.wrappers

import org.meerkat.sppf.NonPackedNode
import org.meerkat.sppf._

import scala.collection.mutable

private class Context(var nodes: Stream[SPPFNode],
                      val queue: mutable.Queue[SPPFNode],
                      var priority: Int) {
  def this(root: SPPFNode) =
    this(Stream(), mutable.Queue(root), 0)
}

private class SPPFToTreesBFSIterator(roots: Seq[NonPackedNode]) extends Iterator[NonPackedNode] {
  implicit private final val ordering = Ordering.by[Context, Int](_.priority)
  val contextQueue = mutable.PriorityQueue(roots.map(root => new Context(root)): _*)
  val cycles = findAllCycles(roots)

  override def hasNext: Boolean = contextQueue.nonEmpty

  override def next(): NonPackedNode = Stream.iterate(step())(_ => step()).flatten.head

  private def step(): Option[NonPackedNode] = {
    val current = contextQueue.dequeue()

    val (root, contexts) = contextStep(current)
    contextQueue.enqueue(contexts: _*)

    root
  }

  private def contextStep(context: Context): (Option[NonPackedNode], Seq[Context]) = {
    if (context.queue.isEmpty)
      (Some(constructTreeFromBFSSequence(context.nodes.reverseIterator)), Seq())
    else {
      val node = context.queue.dequeue()
      context.nodes = node +: context.nodes

      node match {
        case nonpacked: NonPackedNode =>
          val rest = if (nonpacked.rest == null) Seq() else
            nonpacked.rest.map(way => {
              val queue = context.queue.clone()
              queue.enqueue(way)

              new Context(context.nodes, queue, context.priority)
            })

          if (nonpacked.first != null)
            context.queue.enqueue(nonpacked.first)

          (Option.empty, Seq(context) ++ rest)
        case _ =>
          node.children.foreach(child => {
            if (cycles.contains((node, child)))
              context.priority -= 1

            context.queue.enqueue(child)
          })

          (Option.empty, Seq(context))
      }
    }
  }
}

object SPPFToTreesBFSConverter extends SPPFToTreesConverter {
  def apply(roots: Seq[NonPackedNode]): Stream[NonPackedNode] = {
    new SPPFToTreesBFSIterator(roots).toStream
  }
}
