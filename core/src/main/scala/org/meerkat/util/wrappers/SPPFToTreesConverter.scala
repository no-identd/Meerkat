package org.meerkat.util.wrappers

import org.meerkat.sppf.{NonPackedNode, SPPFNode}

trait SPPFToTreesConverter {
  def apply(roots: Seq[NonPackedNode]): Stream[NonPackedNode]
}