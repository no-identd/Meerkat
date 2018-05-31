package org.meerkat.util.converters

import org.meerkat.sppf.{NonPackedNode, SPPFNode}

trait Converter {
  def apply(roots: Seq[NonPackedNode]): Stream[NonPackedNode]
}