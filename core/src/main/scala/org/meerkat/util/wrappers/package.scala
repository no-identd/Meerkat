package org.meerkat.util

import org.meerkat.sppf.NonPackedNode
import org.meerkat.tree.Tree

package object wrappers {
  def SPPFToTrees(root: NonPackedNode): Stream[Tree] = new SPPFToTreesBFSIterator(root).toStream
  def SPPFsToTrees(roots: Seq[NonPackedNode]): Stream[Tree] = new SPPFToTreesBFSIterator(roots).toStream
}
