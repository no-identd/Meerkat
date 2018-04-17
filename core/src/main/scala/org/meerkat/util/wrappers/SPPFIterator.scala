package org.meerkat.util.wrappers

import org.meerkat.sppf._
import org.meerkat.tree
import org.meerkat.tree.{Rule, RuleNode, Tree}

import scala.collection.mutable

private class SubtreeNode(val referent: SPPFNode,
                          val parent: SubtreeNode,
                          val parentRule: Rule,
                          val isLeftChild: Boolean) {
  def this(root: SPPFNode) = this(root, null, null, true)

  var leftChild: SubtreeNode = _
  var rightChild: SubtreeNode = _
  var currentRule: Rule = _

  def toTrees(): Seq[Tree] = {
    referent match {
      case node @ TerminalNode(value, leftExtent, rightExtent) =>
        Seq(tree.TerminalNode(value.toString, leftExtent, rightExtent))

      case node @ IntermediateNode(_, _, _) => {
        val result = leftChild.toTrees() ++ rightChild.toTrees()

        leftChild = null
        rightChild = null
        currentRule = null

        result
      }

      case node @ NonterminalNode(_, _, _) => {
        val leftSeq = if (leftChild != null) leftChild.toTrees() else Seq()
        val rightSeq = if (rightChild != null) rightChild.toTrees() else Seq()
        val result = Seq(RuleNode(currentRule, leftSeq ++ rightSeq))

        leftChild = null
        rightChild = null
        currentRule = null

        result
      }
    }
  }

  def fillUp(): Unit = {
    if (parent != null) {
      if (isLeftChild) {
        parent.leftChild = this
      } else {
        parent.rightChild = this
      }

      parent.currentRule = parentRule;
      parent.fillUp()
    }
  }
}

private class Context(val queue: mutable.Queue[SubtreeNode],
                      val terminals: mutable.ListBuffer[SubtreeNode],
                      root: SubtreeNode) {
  def this(start: SubtreeNode) {
    this(mutable.Queue(start), mutable.ListBuffer(), start)
  }

  override def clone(): Context = {
    new Context(queue.clone(), terminals.clone(), root)
  }

  def extractTree(): Tree = {
    terminals.foreach(_.fillUp())
    root.toTrees().head
  }
}

private class SPPFIterator(roots: Seq[NonPackedNode]) extends Iterator[Context] {
  def this(root: NonPackedNode) = this(Seq(root))

  override def hasNext = !contextQueue.isEmpty

  override def next() =
    Stream.iterate(Option.empty[Context])(_ => step()).flatten.head

  private val contextQueue: mutable.Queue[Context] =
    mutable.Queue(roots.map(root => new Context(new SubtreeNode(root))): _*)

  private def step(): Option[Context] = {
    val currentContext = contextQueue.dequeue()

    if (currentContext.queue.isEmpty) {
      return Some(currentContext)
    } else {
      contextQueue.enqueue(currentContext)

      val node = currentContext.queue.dequeue()
      val referent = node.referent

      if (referent.isInstanceOf[NonterminalNode] || referent.isInstanceOf[IntermediateNode]) {
        val nont = referent.asInstanceOf[NonPackedNode]

        if (nont.rest != null) {
          nont.rest.foreach(way => {
            val newContext = currentContext.clone()
            contextQueue.enqueue(newContext)

            processWay(way, newContext, node)
          })
        }

        processWay(nont.first, currentContext, node)
      } else {
        currentContext.terminals += node
      }
    }

    None
  }

  private def processWay(way: PackedNode, context: Context, parent: SubtreeNode): Unit = {
    if (way.leftChild != null) {
      val child = new SubtreeNode(way.leftChild, parent, way.ruleType, true)
      context.queue.enqueue(child)
    }

    if (way.rightChild != null) {
      val child = new SubtreeNode(way.rightChild, parent, way.ruleType, false)
      context.queue.enqueue(child)
    }
  }
}

object SPPFToTreesStream {
  def apply(root: NonPackedNode): Stream[Tree] = new SPPFIterator(root).map(_.extractTree()).toStream
  def apply(roots: Seq[NonPackedNode]): Stream[Tree] = new SPPFIterator(roots).map(_.extractTree()).toStream
}