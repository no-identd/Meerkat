package org.meerkat.util.converters

import org.meerkat.sppf.NonPackedNode
import org.meerkat.sppf._

import scala.collection.mutable

private class Context(val root: NonPackedNode,
                      var choices: List[SPPFNode],
                      val queue: mutable.Queue[SPPFNode],
                      var priority: Int) {
  def this(_root: NonPackedNode) =
    this(_root, List(), mutable.Queue(_root), 0)
}

private class BFSIterator(roots: Seq[NonPackedNode]) extends Iterator[NonPackedNode] {
  implicit private final val ordering = Ordering.by[Context, Int](_.priority)
  val contextQueue = mutable.PriorityQueue(roots.map(root => new Context(root)): _*)
  val cycles = findAllCycles(roots)

  override def hasNext: Boolean = contextQueue.nonEmpty

  override def next(): NonPackedNode = Stream.iterate(step())(_ => step()).flatten.head

  private def step(): Option[NonPackedNode] = {
    val current = contextQueue.dequeue()

    val (root, contexts) = contextStep(current)
    if (root.isEmpty)
      contextQueue.enqueue(current)
    if (contexts.isDefined)
      contextQueue.enqueue(contexts.get: _*)

    root
  }

  private def contextStep(context: Context): (Option[NonPackedNode], Option[Seq[Context]]) = {
    if (context.queue.isEmpty)
      (Some(constructTreeFromBFSChoices(context.root, context.choices.reverseIterator)), Option.empty)
    else {
      val node = context.queue.dequeue()

      node match {
        case nonpacked: NonPackedNode =>
          if (nonpacked.isAmbiguous) {
            val rest = nonpacked.rest.map(way => {
                val queue = context.queue.clone()
                queue.enqueue(way)

                new Context(context.root, way +: context.choices, queue, context.priority)
              })

            context.choices = nonpacked.first +: context.choices
            context.queue.enqueue(nonpacked.first)

            (Option.empty, Some(rest))
          } else {
            if (nonpacked.first != null)
              context.queue.enqueue(nonpacked.first)

            (Option.empty, Option.empty)
          }
        case _ =>
          node.children.foreach(child => {
            if (cycles.contains((node, child)))
              context.priority -= 1

            context.queue.enqueue(child)
          })

          (Option.empty, Option.empty)
      }
    }
  }
}

object BFSConverter extends Converter {
  def apply(roots: Seq[NonPackedNode]): Stream[NonPackedNode] = {
    new BFSIterator(roots).toStream
  }
}
