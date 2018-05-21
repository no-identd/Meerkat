package org.meerkat.util.wrappers
import org.meerkat.sppf._

object SPPFToTreesEnumeratingConverter extends SPPFToTreesConverter {
   def apply(roots: Seq[NonPackedNode], countTreesBeforeExtraction: Boolean): Stream[NonPackedNode] = {
     val stream = Stream.iterate(0)(_ + 1)
       .flatMap(i => roots.map(root => process(root, i)))
       .flatten

     if (countTreesBeforeExtraction) {
       val counts = roots.toStream.map(tryToCountTrees)
       val finite = counts.forall(_.isSuccess)

       if (finite) {
         val sum = counts.map(_.get).sum
         println(sum)

         return stream.take(sum)
       }
     }

     stream
   }

  override def apply(roots: Seq[NonPackedNode]): Stream[NonPackedNode] = apply(roots, true)

  private def process(root: NonPackedNode, id: Int): Option[NonPackedNode] = {
    val (sequence, residual) = getDFSSequence(root, id)

    if (residual != 1)
      return Option.empty

    return Option(constructTreeFromDFSSequence(sequence.toIterator))
  }

  private def getDFSSequence(root: SPPFNode, id: Int): (Seq[SPPFNode], Int) = {
    var currentId = id

    val next = root match {
      case terminal: TerminalNode[_] => Seq.empty[SPPFNode]
      case packed: PackedNode => {
        packed.children.flatMap(child => {
          val (stream, newId) = getDFSSequence(child, currentId)
          currentId = newId
          stream
        })
      }
      case nonpacked: NonPackedNode => {
        val alternatives = nonpacked.children.size;
        if (id < alternatives)
          return (Seq.empty[SPPFNode], 0)

        val (stream, newId) = getDFSSequence(nonpacked.children(id % alternatives), id / alternatives);
        currentId = newId
        stream
      }
    }

    (root +: next, currentId)
  }
}
