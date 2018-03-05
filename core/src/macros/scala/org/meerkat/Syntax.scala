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

package org.meerkat

import org.meerkat.sppf.NonPackedNode
import org.meerkat.parsers.&
import org.meerkat.parsers.Parsers.Symbol
import org.meerkat.parsers.Parsers
import org.meerkat.parsers.Parsers.AlternationBuilder
import org.meerkat.parsers.Parsers.SequenceBuilder
import org.meerkat.parsers.OperatorParsers.OperatorAlternationBuilder
import org.meerkat.parsers.OperatorParsers.OperatorSequenceBuilderWithAction
import org.meerkat.parsers.OperatorParsers.AbstractOperatorNonterminal
import org.meerkat.parsers.OperatorParsers.OperatorNonterminalWithAction
import org.meerkat.parsers.Parsers.Nonterminal
import org.meerkat.parsers.OperatorParsers.OperatorSequenceBuilder
import org.meerkat.parsers.OperatorParsers.OperatorNonterminal
import org.bitbucket.inkytonik.dsinfo.DSInfo.makeCallWithName
import org.meerkat.Syntax.makeNegativeSymWithName
import org.meerkat.parsers.AbstractCPSParsers.AbstractSymbol

import scala.reflect.macros.blackbox.Context
import org.meerkat.parsers.NoValue

object Syntax {
  import scala.language.experimental.macros
  import scala.reflect.macros.blackbox.Context

  def syn[E, N, T](p: Parsers.AlternationBuilder[E, N, T]): Nonterminal[E, N] & T= macro makeNonterminalAltWithName[E, N, T]
  def syn[E, N, T](p: Parsers.SequenceBuilder[E, N, T]): Nonterminal[E, N] & T= macro makeNonterminalSeqWithName[E, N, T]
  def syn[E, N, T](p: AbstractSymbol[E, N, NonPackedNode, T]): Nonterminal[E, N] & T= macro makeNonterminalSymWithName[E, N, T]
  def syn[E, N, T](p: Symbol[E, N, T]): Nonterminal[E, N] & T= macro makeNonterminalSymWithName[E, N, T]

  def not[E, N, T](p: AbstractSymbol[E, N, NonPackedNode, T]): Nonterminal[E, N] & T= macro makeNegativeSymWithName[E, N, T]

  def makeNonterminalAltWithName[E, N, T](c: Context)(p: c.Expr[AlternationBuilder[E, N, T]]): c.Expr[Nonterminal[E, N] & T] =
    makeCallWithName(c, "Parsers.ntAlt")
  def makeNonterminalSeqWithName[E, N, T](c: Context)(p: c.Expr[SequenceBuilder[E, N, T]]): c.Expr[Nonterminal[E, N] & T] =
    makeCallWithName(c, "Parsers.ntSeq")
  def makeNonterminalSymWithName[E, N, T](
    c: Context
  )(p: c.Expr[AbstractSymbol[E, N, NonPackedNode, T]]): c.Expr[Nonterminal[E, N] & T] =
    makeCallWithName(c, "Parsers.ntSym")

  def makeNegativeSymWithName[E, N, T](
    c: Context
  )(p: c.Expr[AbstractSymbol[E, N, NonPackedNode, T]]): c.Expr[Nonterminal[E, N] & T] =
    makeCallWithName(c, "Parsers.notSym")

  def syn[E, N, T](p: OperatorAlternationBuilder[E, N, T]): OperatorNonterminal[E, N] & T=
    macro makeOperatorNonterminalAltWithName[E, N, T]
  def syn[E, N, T](p: OperatorSequenceBuilder[E, N, T]): OperatorNonterminal[E, N] & T=
    macro makeOperatorNonterminalSeqWithName[E, N, T]
  def syn[E, N, T](p: AbstractOperatorNonterminal[E, N, T]): OperatorNonterminal[E, N] & T=
    macro makeOperatorNonterminalSymWithName[E, N, T]
  def syn[E, N, T](p: OperatorSequenceBuilderWithAction[E, N, T]): OperatorNonterminal[E, N] & T=
    macro makeOperatorNonterminalSeqWithActionWithName[E, N, T]
  def syn[E, N, T](p: OperatorNonterminalWithAction[E, N,T]): OperatorNonterminal[E, N] & T=
    macro makeOperatorNonterminalSymWithActionWithName[E, N, T]

  def makeOperatorNonterminalAltWithName[E, N, T](c: Context)(
    p: c.Expr[OperatorAlternationBuilder[E, N, T]]
  ): c.Expr[OperatorNonterminal[E, N] & T] = makeCallWithName(c, "OperatorParsers.ntAlt")
  def makeOperatorNonterminalSeqWithName[E, N, T](c: Context)(
    p: c.Expr[OperatorSequenceBuilder[E, N, T]]
  ): c.Expr[OperatorNonterminal[E, N] & T] = makeCallWithName(c, "OperatorParsers.ntSeq")
  def makeOperatorNonterminalSymWithName[E, N, T](c: Context)(
    p: c.Expr[AbstractOperatorNonterminal[E, N, T]]
  ): c.Expr[OperatorNonterminal[E, N] & T] = makeCallWithName(c, "OperatorParsers.ntSym")
  def makeOperatorNonterminalSeqWithActionWithName[E, N, T](c: Context)(
    p: c.Expr[OperatorSequenceBuilderWithAction[E, N, T]]
  ): c.Expr[OperatorNonterminal[E, N] & T] = makeCallWithName(c, "OperatorParsers.ntSeqWithAction")
  def makeOperatorNonterminalSymWithActionWithName[E, N, T](c: Context)(
    p: c.Expr[OperatorNonterminalWithAction[E, N,T]]
  ): c.Expr[OperatorNonterminal[E, N] & T] = makeCallWithName(c, "OperatorParsers.ntSymWithAction")
}
