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

import scala.util.matching.Regex
import scala.collection.mutable._
import scala.collection.immutable.Set
import scala.collection.JavaConversions._
import scala.collection.mutable
import scalax.collection.Graph
import scalax.collection.GraphPredef.{EdgeLikeIn, Param}
import scalax.collection.edge.LDiEdge

trait Input {
  def start: Int = 0
  def length: Int

  val lineColumns: Array[(Int, Int)]
  val regexMap: Map[Regex, java.util.regex.Matcher]

  def charAt(i: Int): scala.Char

  def substring(start: Int, end: Int): String

  def startsWith(prefix: String, toffset: Int): Option[Set[Int]]

  //Boolean
  def endsWith(suffix: String): Boolean

  def matchRegex(r: Regex, start: Int, end: Int): Boolean

  def matchRegex(r: Regex, start: Int): Set[Int]
}

class InputString(val s: String) extends Input {

  def length = {
    var l = s.length()
    l
  }

  val lineColumns: Array[(Int, Int)] = Array.ofDim[(Int, Int)](length + 1)

  val regexMap: Map[Regex, java.util.regex.Matcher] = new java.util.HashMap[Regex, java.util.regex.Matcher]()

  calcLineColumns

  def calcLineColumns: Unit = {
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

  def charAt(i: Int): scala.Char = {
    val c = s.charAt(i)
    c
  }

  def substring(start: Int, end: Int): String = {
    val c = s.substring(start, end)
    c
  }

  def startsWith(prefix: String, toffset: Int): Option[Set[Int]] = {
    val w = s.startsWith(prefix, toffset)
    if (w)
      Some(Set(toffset + prefix.length))
    else None
  }

  def endsWith(suffix: String) = {
    val w = s.endsWith(suffix)
    w
  }

  def matchRegex(r: Regex, start: Int, end: Int): Boolean = {
    if (start < 0) return false
    val matcher = regexMap.getOrElse(r, {
      val matcher: java.util.regex.Matcher = r.pattern.matcher(s); matcher
    })
    matcher.region(start, end)
    return matcher.matches()
  }

  def matchRegex(r: Regex, start: Int): Set[Int] = {
    if (start < 0) return Set() //-1
    val matcher = regexMap.getOrElse(r, {
      val matcher: java.util.regex.Matcher = r.pattern.matcher(s); matcher
    })
    matcher.region(start, length)
    if (matcher.lookingAt()) Set(matcher.end) else Set() //-1
  }

  def lineNumber(i: Int): Int = lineColumns(i)._1

  def columnNumber(i: Int): Int = lineColumns(i)._2

  def lineColumn(i: Int): (Int, Int) = lineColumns(i)

}

class InputGraph(g: IGraph, startParsing: Int = 0) extends Input {
  override def start = startParsing
  private def n(outer: Int): INode = g get outer

  def length: Int = g.countNodes

  val lineColumns: Array[(Int, Int)] = Array.ofDim[(Int, Int)](length + 1)
  val regexMap: Map[Regex, java.util.regex.Matcher] = new java.util.HashMap[Regex, java.util.regex.Matcher]()

  def charAt(i: Int): scala.Char = {
    val f = n(i).getOutgoingEdges.head.getLabel.charAt(0) //.edges.head.label.toString().charAt(0)
    f
  }

  def substring(start: Int, end: Int): String = {
    val edges = n(start).getOutgoingEdges.filter(e => e.getToNode.value == end)
    if (edges.nonEmpty) {
      println(edges.head.getLabel); edges.head.getLabel
    }
    else {
      println("substring"); ""
    }
  }

  def startsWith(prefix: String, toffset: Int): Option[Set[Int]] = {
    val v = if(toffset == Int.MinValue) 0 else Math.abs(toffset)
    var i = n(v)
    val res = mutable.Set[Int]()
    val sourse = if(toffset > 0) i.getOutgoingEdges else i.getIncomingEdges
    val edges = sourse.filter(x => {
      x.getLabel.equals(prefix.toString)
    })
    if (edges.nonEmpty) {
      for (edge <- edges) res += (if(toffset < 0) edge.getFromNode.value else edge.getToNode.value)
      Some(res.toSet)
    }
    else None
  }

  def endsWith(suffix: String): Boolean = {
    val res = n(this.length - 1).getIncomingEdges.filter { x => x.getLabel.toString() == suffix }
    res.nonEmpty
  }

  def matchRegex(r: Regex, start: Int, end: Int): Boolean = {
    if (start < 0) return false
    n(start).getOutgoingEdges.filter(s => s.getToNode.value == end).exists(s => {
      val matcher = regexMap.getOrElse(r, {
        val matcher: java.util.regex.Matcher = r.pattern.matcher(s.getLabel); matcher
      })
      matcher.region(0, s.getLabel.length)
      matcher.matches()
    })
  }

  def matchRegex(r: Regex, start: Int): Set[Int] = {
    if (start < 0) return collection.immutable.Set[Int]()
    n(start).getOutgoingEdges.filter(s => {
      val matcher = regexMap.getOrElse(r, {
        val matcher: java.util.regex.Matcher = r.pattern.matcher(s.getLabel); matcher
      })
      matcher.region(0, s.getLabel.length)
      matcher.matches()
    }).map(e => e.getToNode.value)
  }
}

object Input {

  def apply(s: String) = new InputString(s)

  def apply(s: IGraph, start: Int) = new InputGraph(s, start)

  def apply(s: IGraph) = new InputGraph(s, 0)

  //def apply(s: Graph[Int,LDiEdge]) = new InputGraph(s)
  implicit def toInput(s: IGraph, start: Int) = Input(s, start)
  implicit def toInput(s: IGraph) = Input(s, 0)

  //implicit def toInput(s: Graph[Int,LDiEdge]) = Input(s)
  implicit def toInput(s: String) = Input(s)
}
