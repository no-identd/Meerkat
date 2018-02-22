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

package org.meerkat.util

import scala.collection.immutable.HashMap
import scala.collection.mutable
import scala.language.implicitConversions
import scala.util.matching.Regex

trait Input[Edge, Node] {
  def length: Int

  def start: Int = 0

  def filterEdges(node: Node, label: String): collection.Seq[Int]

  def filterEdges(id: Int, label: String): collection.Seq[Int]  =
    filterEdges(nodeById(id), label)

  def idByNode(node: Node): Int

  def nodeById(id: Int): Node

//  def to(edge: Edge): Node
}

class LinearInput(string: String) extends Input[String, Int] {
  override def filterEdges(node: Int, label: String): scala.Seq[Int] =
    if (string.startsWith(label, node)) collection.Seq(node + label.length)
    else collection.Seq.empty

  override def idByNode(node: Int): Int = node

  override def nodeById(id: Int): Int = id

//  override def to(edge: String): Int = ???
  override def length: Int = string.length
}

object Input {
  def apply(s: String) = new LinearInput(s)

  implicit def toInput(s: String): LinearInput = Input(s)
}
