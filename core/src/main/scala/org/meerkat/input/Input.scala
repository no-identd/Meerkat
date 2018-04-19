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

package org.meerkat.input

import scala.language.implicitConversions


trait Input[-L] {
  type M >: L
  type Edge = (M, Int)

  def length: Int

  def start: Int = 0

  def filterEdges(nodeId: Int, predicate: M => Boolean): collection.Seq[Edge]

  def filterEdges(nodeId: Int, label: M): collection.Seq[Edge] =
    filterEdges(nodeId, (_: M) == label)

  def outEdges(nodeId: Int): collection.Seq[Edge] =
    filterEdges(nodeId, (_: M) => true)

  def checkNode(nodeId: Int, label: M): Boolean =
    checkNode(nodeId, (_: M) == label)

  def checkNode(nodeId: Int, predicate: M => Boolean): Boolean

  /// TODO: get rid of it
  def substring(start: Int, end: Int): String =
    throw new RuntimeException("Not supported")

  def epsilonLabel: Any

  def charAt(i: Int): Char =
    throw new RuntimeException("Not supported")
}

