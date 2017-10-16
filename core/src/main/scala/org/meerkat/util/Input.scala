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

trait Input {
  def start: Int = 0

  def length: Int

  def charAt(i: Int): scala.Char

  def substring(start: Int, end: Int): String

  def startsWith(prefix: String, toffset: Int): Set[Int]

  def endsWith(suffix: String): Boolean

  def matchRegex(r: Regex, start: Int, end: Int): Boolean

  def matchRegex(r: Regex, start: Int): Set[Int]
}

class InputString(val s: String) extends Input {
  private val lineColumns: Array[(Int, Int)] = Array.ofDim[(Int, Int)](length + 1)
  private val regexMap: Map[Regex, java.util.regex.Matcher] = Map.empty

  def calcLineColumns(): Unit = {
    var lineCount = 0
    var lineNumber = 1
    var columnNumber = 1

    // Empty input: only the end of line symbol
    if (length == 1) {
      lineColumns(0) = (lineNumber, columnNumber)
    } else {
      for (i <- 0 until length) {
        lineColumns(i) = (lineNumber, columnNumber)
        if (s.charAt(i) == '\n') {
          lineCount += 1
          lineNumber += 1
          columnNumber = 1
        } else if (s.charAt(i) == '\r') {
          columnNumber = 1
        } else {
          columnNumber += 1
        }
      }
    }
    lineColumns(length) = (lineNumber, columnNumber)
  }

  calcLineColumns()

  def length: Int = s.length

  def charAt(i: Int): scala.Char = s.charAt(i)

  def substring(start: Int, end: Int): String = s.substring(start, end)

  def startsWith(prefix: String, toffset: Int): Set[Int] = {
    if (s.startsWith(prefix, toffset)) Set(toffset + prefix.length)
    else Set.empty
  }

  def endsWith(suffix: String): Boolean = s.endsWith(suffix)

  def matchRegex(r: Regex, start: Int, end: Int): Boolean = {
    if (start < 0) return false
    val matcher = regexMap.getOrElse(r, r.pattern.matcher(s))
    matcher.region(start, end)
    matcher.matches()
  }

  def matchRegex(r: Regex, start: Int): Set[Int] = {
    if (start < 0) return Set() //-1
    val matcher = regexMap.getOrElse(r, r.pattern.matcher(s))
    matcher.region(start, length)
    if (matcher.lookingAt()) Set(matcher.end) else Set() //-1
  }

  def lineNumber(i: Int): Int = lineColumns(i)._1

  def columnNumber(i: Int): Int = lineColumns(i)._2

  def lineColumn(i: Int): (Int, Int) = lineColumns(i)
}

class InputGraph(g: IGraph, startParsing: Int = 0) extends Input {
  private val lineColumns: Array[(Int, Int)] = Array.ofDim[(Int, Int)](length + 1)
  private val regexMap: Map[Regex, java.util.regex.Matcher] = HashMap[Regex, java.util.regex.Matcher]()

  override def start: Int = startParsing

  override def charAt(i: Int): scala.Char =
    node(i).outgoingEdges.head.label.charAt(0)

  override def substring(start: Int, end: Int): String = {
    val edges = node(start).outgoingEdges.filter(_.to.value == end)
    if (edges.nonEmpty) edges.head.label
    else ""
  }

  override def startsWith(prefix: String, toffset: Int): Set[Int] = {
    val v = if (toffset == Int.MinValue) 0 else Math.abs(toffset)
    val i = node(v)
    val res = mutable.Set[Int]()
    val sourse = if (toffset >= 0) i.outgoingEdges else i.incomingEdges
    val edges = sourse.filter(x => x.label.equals(prefix.toString))
    if (edges.nonEmpty) {
      for (edge <- edges) res += (if (toffset < 0) edge.from.value else edge.to.value)
      res.toSet
    }
    else Set.empty
  }

  override def endsWith(suffix: String): Boolean = {
    val res = node(length - 1).incomingEdges.filter(_.label == suffix)
    res.nonEmpty
  }

  private def node(outer: Int): INode = g.get(outer)

  override def length: Int = g.nodesCount

  def matchRegex(r: Regex, start: Int, end: Int): Boolean = {
    if (start < 0) return false
    node(start).outgoingEdges.filter(s => s.to.value == end).exists(s => {
      val matcher = regexMap.getOrElse(r, r.pattern.matcher(s.label))
      matcher.region(0, s.label.length)
      matcher.matches()
    })
  }

  override def matchRegex(r: Regex, start: Int): Set[Int] = {
    if (start < 0) return Set.empty
    node(start).outgoingEdges.filter(s => {
      val matcher = regexMap.getOrElse(r, r.pattern.matcher(s.label))
      matcher.region(0, s.label.length)
      matcher.matches()
    }).map(_.to.value)
  }
}

object Input {

  def apply(s: IGraph) = new InputGraph(s, 0)

  def apply(s: String) = new InputString(s)

  def apply(s: IGraph, start: Int) = new InputGraph(s, start)

  implicit def toInput(s: IGraph, start: Int): InputGraph = Input(s, start)

  implicit def toInput(s: IGraph): InputGraph = Input(s, 0)

  implicit def toInput(s: String): InputString = Input(s)
}
