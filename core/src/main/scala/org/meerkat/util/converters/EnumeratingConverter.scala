package org.meerkat.util.converters
import org.meerkat.sppf._

object EnumeratingConverter extends Converter {
   def apply(roots: Seq[NonPackedNode], countTreesBeforeExtraction: Boolean): Stream[NonPackedNode] = {
     lazy val stream = Stream.iterate(1)(_ + 1)
       .flatMap(i => roots.map(root => process(root, i)))
       .flatten

     if (countTreesBeforeExtraction) {
       val counts = roots.map(tryToCountTrees)
       val finite = counts.forall(_.isSuccess)

       if (finite) {
         val sum = counts.map(_.get).sum

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

    return Option(constructTreeFromDFSChoices(root, sequence.toIterator))
  }

  private def getDFSSequence(root: SPPFNode, id: Int): (Seq[SPPFNode], Int) = {
    var currentId = id

    val sequence = root match {
      case terminal: TerminalNode[_] => Seq.empty[SPPFNode]
      case vertex: VertexNode[_] => Seq.empty[SPPFNode]
      case epsilon: EpsilonNode => Seq.empty[SPPFNode]
      case packed: PackedNode => {
        packed.children.flatMap(child => {
          val (stream, newId) = getDFSSequence(child, currentId)
          currentId = newId
          stream
        })
      }
      case nonpacked: NonPackedNode => {
        val alternatives = nonpacked.children.size
        if (id < alternatives)
          return (Seq.empty[SPPFNode], 0)

        val nextChild = nonpacked.children(id % alternatives)
        val (stream, newId) = getDFSSequence(nextChild, id / alternatives);
        currentId = newId

        if (nonpacked.isAmbiguous)
          nextChild +: stream
        else
          stream
      }
    }

    (sequence, currentId)
  }
}
