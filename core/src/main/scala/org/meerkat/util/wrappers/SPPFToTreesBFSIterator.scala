package org.meerkat.util.wrappers

import org.meerkat.sppf._
import org.meerkat.tree
import org.meerkat.tree.{Rule, RuleNode, Tree}

import scala.collection.mutable

private class SubtreeNode(referent: SPPFNode) {
  var cloneRef: SubtreeNode = _

  var leftChild: SubtreeNode = _
  var rightChild: SubtreeNode = _

  var rule: Rule = _

  def this(referent: SPPFNode, leftChild: SubtreeNode, rightChild: SubtreeNode, rule: Rule) {
    this(referent)
    this.leftChild = leftChild
    this.rightChild = rightChild
    this.rule = rule
  }

  def getReferent = referent

  override def clone(): SubtreeNode = {
    val leftClone = if (leftChild != null) leftChild.clone() else null
    val rightClone = if (rightChild != null) rightChild.clone() else null
    val result = new SubtreeNode(referent, leftClone, rightClone, rule)

    cloneRef = result

    result
  }

  def toTrees(): Seq[Tree] = {
    referent match {
      case node @ TerminalNode(value, _, _) =>
        Seq(tree.TerminalNode(value.toString))

      case node @ IntermediateNode(_, _, _) =>
        leftChild.toTrees() ++ rightChild.toTrees()

      case node @ NonterminalNode(_, _, _) => {
        val leftSeq = if (leftChild != null) leftChild.toTrees() else Seq()
        val rightSeq = if (rightChild != null) rightChild.toTrees() else Seq()
        Seq(RuleNode(rule, leftSeq ++ rightSeq))
      }
    }
  }
}

private class Context(startQueue: mutable.Queue[SubtreeNode], startRoot: SubtreeNode) {
  val queue = startQueue
  val root = startRoot

  def this(start: SubtreeNode) {
    this(mutable.Queue(start), start)
  }

  override def clone(): Context = {
    val clonedTree = root.clone()
    val clonedQueue = queue.clone().map(_.cloneRef)
    new Context(clonedQueue, clonedTree)
  }
}

class SPPFToTreesBFSIterator(root: NonPackedNode) extends Iterator[Tree] {
  override def hasNext: Boolean = !contextQueue.isEmpty

  override def next(): Tree =
    Stream.iterate(Option.empty[SubtreeNode])(_ => step()).
      flatten.head.toTrees.head

  private val contextQueue: mutable.Queue[Context] =
    mutable.Queue(new Context(new SubtreeNode(root)))

  private def step(): Option[SubtreeNode] = {
    val currentContext = contextQueue.dequeue()

    if (currentContext.queue.isEmpty) {
      return Some(currentContext.root)
    } else {
      contextQueue.enqueue(currentContext)

      val node = currentContext.queue.dequeue()
      val referent = node.getReferent

      if (referent.isInstanceOf[NonterminalNode] || referent.isInstanceOf[IntermediateNode]) {
        val nonp = referent.asInstanceOf[NonPackedNode]

        if (nonp.rest != null) {
          nonp.rest.foreach(way => {
            val newContext = currentContext.clone()
            val newNode = node.cloneRef

            processWay(way, newContext, newNode)
            contextQueue.enqueue(newContext)
          })
        }

        processWay(nonp.first, currentContext, node);
      }
    }

    None
  }

  private def processWay(way: PackedNode, context: Context, node: SubtreeNode): Unit = {
    if (way.leftChild != null) {
      node.leftChild = new SubtreeNode(way.leftChild)
      context.queue.enqueue(node.leftChild)
    }

    if (way.rightChild != null) {
      node.rightChild = new SubtreeNode(way.rightChild)
      context.queue.enqueue(node.rightChild)
    }

    node.rule = way.ruleType
  }
}

