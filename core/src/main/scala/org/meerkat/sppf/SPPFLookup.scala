/*
 * Copyright (c) 2015, Anastasia Izmaylova and Ali Afroozeh, Centrum Wiskunde & Informatica (CWI)
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice, this
 *    list of conditions and the following disclaimer in the documentation and/or
 *    other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
 * OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY
 * OF SUCH DAMAGE.
 *
 */

package org.meerkat.sppf

import org.meerkat.input.Input
import org.meerkat.parsers.Parsers

import scala.collection.mutable
import scala.collection.immutable.Set
import org.meerkat.util.IntKey3

//TODO: add vertex nodes for maping??
trait SPPFLookup[L, N] {
  def getStartNode(name: Any, leftExtent: Int, rightExtent: Int): Option[NonPackedNode]
  def getEdgeNode[F <: L](s: F, leftExtent: Int, rightExtent: Int, out: Boolean): EdgeNode[F]
  def getVertexNode(s: N, extent: Int): VertexNode[N]
  def getEpsilonNode(inputIndex: Int): EpsilonNode
  def getNonterminalNode(head: Any,
                         slot: Slot,
                         leftChild: Option[NonPackedNode],
                         rightChild: NonPackedNode): NonPackedNode
  def getNonterminalNode(head: Any, slot: Slot, rightChild: NonPackedNode): NonPackedNode
  def getIntermediateNode(slot: Slot, leftChild: Option[NonPackedNode], rightChild: NonPackedNode): NonPackedNode
  def getIntermediateNode(slot: Slot, leftChild: NonPackedNode, rightChild: NonPackedNode): NonPackedNode
  def countNonterminalNodes: Int
  def countIntermediateNodes: Int
  def countPackedNodes: Int
  def countEdgeNodes: Int
  def countAmbiguousNodes: Int
}

class DefaultSPPFLookup[L, N](input: Input[L, N]) extends SPPFLookup[L, N] {

  private val n    = input.edgesCount
  private val hash = (k1: Int, k2: Int, k3: Int) => k1 * n * n + k2 * n + k3
  // TODO: get rid of ANY
  val edgeNodes: mutable.Map[IntKey3, EdgeNode[Any]] = mutable.HashMap()
  val vertexNodes: mutable.Map[IntKey3, VertexNode[Any]] = mutable.HashMap()
  val epsilonNodes: mutable.Map[IntKey3, EpsilonNode]        = mutable.HashMap()
  val nonterminalNodes: mutable.Map[IntKey3, NonPackedNode]  = mutable.HashMap[IntKey3, NonPackedNode]()
  val intermediateNodes: mutable.Map[IntKey3, NonPackedNode] = mutable.HashMap[IntKey3, NonPackedNode]()

  var countNonterminalNodes: Int  = 0
  var countIntermediateNodes: Int = 0
  var countPackedNodes: Int       = 0
  var countEdgeNodes: Int     = 0
  var countVertexNodes: Int       = 0
  var countAmbiguousNodes: Int    = 0

  override def getStartNode(name: Any, leftExtent: Int, rightExtent: Int): Option[NonPackedNode] = {
    //for(i <- 0 to rightExtent) {
    nonterminalNodes.get(IntKey3(name.hashCode(), leftExtent, rightExtent, hash)) match {
      case None       =>
      case Some(root) => return Some(root)
      //}

    }
    None
  }
  def getStartNodesFilterByStarts(name: Any, starts: Set[Int]): Option[List[NonPackedNode]] =
    nonterminalNodes.values
      .filter((node) => node.name == name && starts.contains(node.leftExtent))
      .toList match {
      case Nil   => None
      case roots => Some(roots)
    }
  def getStartNodesFilterByEnds(name: Any, ends: Set[Int]): Option[List[NonPackedNode]] =
    nonterminalNodes.values
      .filter(node => node.name == name && ends.contains(node.rightExtent))
      .toList match {
      case Nil   => None
      case roots => Some(roots)
    }
  def getStartNodesFilterByStartAndEnds(name: Any, starts: Set[Int], ends: Set[Int]): Option[List[NonPackedNode]] =
    nonterminalNodes.values
      .filter(
        node =>
          node.name.equals(name)
            && starts.contains(node.leftExtent)
            && ends.contains(node.rightExtent)
      )
      .toList match {
      case Nil   => None
      case roots => Some(roots)
    }

  def getEdgeNode[F <: L](s: F, leftExtent: Int, rightExtent: Int, out: Boolean): EdgeNode[F] =
    findOrElseCreateTerminalNode(s, index(leftExtent), index(rightExtent), out)

  def getEpsilonNode(inputIndex: Int): EpsilonNode = {
    val i = index(inputIndex)
    findOrElseCreateEpsilonNode(i)
  }

  def getNonterminalNode(head: Any,
                         slot: Slot,
                         leftChild: Option[NonPackedNode],
                         rightChild: NonPackedNode): NonPackedNode = {

    val leftExtent  = if (leftChild.isDefined) leftChild.get.leftExtent else rightChild.leftExtent
    val rightExtent = rightChild.rightExtent
    val node        = findOrElseCreateNonterminalNode(head, leftExtent, rightExtent)

    val packedNode = PackedNode(slot, node)

    val ambiguousBefore = node.isAmbiguous
    if (node.addPackedNode(packedNode, leftChild, rightChild)) countPackedNodes += 1
    val ambiguousAfter = node.isAmbiguous

    if (!ambiguousBefore && ambiguousAfter) {
      countAmbiguousNodes += 1
    }

    node
  }

  def getNonterminalNode(head: Any, slot: Slot, rightChild: NonPackedNode): NonPackedNode =
    getNonterminalNode(head, slot, None, rightChild)

  def getIntermediateNode(slot: Slot, leftChild: Option[NonPackedNode], rightChild: NonPackedNode): NonPackedNode = {

    val leftExtent  = (if (leftChild.isDefined) leftChild.get else rightChild).leftExtent
    val rightExtent = rightChild.rightExtent
    val node        = findOrElseCreateIntermediateNode(slot, leftExtent, rightExtent)

    val packedNode = PackedNode(slot, node)

    val ambiguousBefore = node.isAmbiguous
    if (node.addPackedNode(packedNode, leftChild, rightChild)) countPackedNodes += 1
    val ambiguousAfter = node.isAmbiguous

    if (!ambiguousBefore && ambiguousAfter) countAmbiguousNodes += 1

    node
  }

  def getIntermediateNode(slot: Slot, leftChild: NonPackedNode, rightChild: NonPackedNode): NonPackedNode =
    getIntermediateNode(slot, Some(leftChild), rightChild)

  def findOrElseCreateTerminalNode[F <: L](s: F, leftExtent: Int, rightExtent: Int, out: Boolean): EdgeNode[F] = {
    val dir = if (out) 1 else -1
    val key = IntKey3(s.hashCode(), leftExtent * dir, rightExtent, hash)
    edgeNodes.getOrElseUpdate(key, {
      countEdgeNodes += 1
      EdgeNode(s, leftExtent, rightExtent, out).asInstanceOf[EdgeNode[Any]]
    }).asInstanceOf[EdgeNode[F]]
  }

  def findOrElseCreateVertexNode(s: N, extent: Int): VertexNode[N] = {
    val key = IntKey3(s.hashCode(), extent, extent, hash)
    vertexNodes.getOrElseUpdate(key, {
      countVertexNodes += 1
      VertexNode(s, extent).asInstanceOf[VertexNode[Any]]
    }).asInstanceOf[VertexNode[N]]
  }


  def findOrElseCreateEpsilonNode(extent: Int): EpsilonNode = {
    val key = IntKey3("epsilon".hashCode(), extent, extent, hash)
    epsilonNodes.getOrElseUpdate(key, {
      countEdgeNodes += 1
      EpsilonNode(extent)
    })
  }

  def findOrElseCreateNonterminalNode(slot: Any, leftExtent: Int, rightExtent: Int): NonPackedNode = {
    val key = IntKey3(slot.hashCode(), leftExtent, rightExtent, hash)
    nonterminalNodes.getOrElseUpdate(
      key, { countNonterminalNodes += 1; NonterminalNode(slot, leftExtent, rightExtent) }
    )
  }

  def findOrElseCreateIntermediateNode(slot: Any, leftExtent: Int, rightExtent: Int): NonPackedNode = {
    val key = IntKey3(slot.hashCode(), leftExtent, rightExtent, hash)
    intermediateNodes.getOrElseUpdate(key, {
      countIntermediateNodes += 1; IntermediateNode(slot, leftExtent, rightExtent)
    })
  }

  def findNonterminalsByName(name: String): Seq[NonterminalNode] =
    nonterminalNodes.values.collect {
      case n @ NonterminalNode(nt: Parsers.AbstractNonterminal[L, _, _], _, _) if nt.name == name => n
    }.toSeq

  private def index(i: Int): Int = i match {
    case Int.MinValue => 0
    case _            => Math.abs(i)
  }

  override def getVertexNode(s: N, extent: Int): VertexNode[N] =
    findOrElseCreateVertexNode(s, index(extent))
}
