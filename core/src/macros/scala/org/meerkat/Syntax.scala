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

  def syn[L, N, T](p: Parsers.AlternationBuilder[L, N, T]): Nonterminal[L, N] & T= macro makeNonterminalAltWithName[L, N, T]
  def syn[L, N, T](p: Parsers.SequenceBuilder[L, N, T]): Nonterminal[L, N] & T= macro makeNonterminalSeqWithName[L, N, T]
  def syn[L, N, T](p: AbstractSymbol[L, N, NonPackedNode, T]): Nonterminal[L, N] & T= macro makeNonterminalSymWithName[L, N, T]
//  def syn[L, N, T](p: Symbol[L, N, T]): Nonterminal[L, N] & T= macro makeNonterminalSymWithName[L, N, T]

  def not[L, N, T](p: AbstractSymbol[L, N, NonPackedNode, T]): Nonterminal[L, N] & T= macro makeNegativeSymWithName[L, N, T]

  def makeNonterminalAltWithName[L, N, T](c: Context)(p: c.Expr[AlternationBuilder[L, N, T]]): c.Expr[Nonterminal[L, N] & T] =
    makeCallWithName(c, "Parsers.ntAlt")
  def makeNonterminalSeqWithName[L, N, T](c: Context)(p: c.Expr[SequenceBuilder[L, N, T]]): c.Expr[Nonterminal[L, N] & T] =
    makeCallWithName(c, "Parsers.ntSeq")
  def makeNonterminalSymWithName[L, N, T](
    c: Context
  )(p: c.Expr[AbstractSymbol[L, N, NonPackedNode, T]]): c.Expr[Nonterminal[L, N] & T] =
    makeCallWithName(c, "Parsers.ntSym")

  def makeNegativeSymWithName[L, N, T](
    c: Context
  )(p: c.Expr[AbstractSymbol[L, N, NonPackedNode, T]]): c.Expr[Nonterminal[L, N] & T] =
    makeCallWithName(c, "Parsers.notSym")

  def syn[L, N, T](p: OperatorAlternationBuilder[L, N, T]): OperatorNonterminal[L, N] & T=
    macro makeOperatorNonterminalAltWithName[L, N, T]
  def syn[L, N, T](p: OperatorSequenceBuilder[L, N, T]): OperatorNonterminal[L, N] & T=
    macro makeOperatorNonterminalSeqWithName[L, N, T]
  def syn[L, N, T](p: AbstractOperatorNonterminal[L, N, T]): OperatorNonterminal[L, N] & T=
    macro makeOperatorNonterminalSymWithName[L, N, T]
  def syn[L, N, T](p: OperatorSequenceBuilderWithAction[L, N, T]): OperatorNonterminal[L, N] & T=
    macro makeOperatorNonterminalSeqWithActionWithName[L, N, T]
  def syn[L, N, T](p: OperatorNonterminalWithAction[L, N,T]): OperatorNonterminal[L, N] & T=
    macro makeOperatorNonterminalSymWithActionWithName[L, N, T]

  def makeOperatorNonterminalAltWithName[L, N, T](c: Context)(
    p: c.Expr[OperatorAlternationBuilder[L, N, T]]
  ): c.Expr[OperatorNonterminal[L, N] & T] = makeCallWithName(c, "OperatorParsers.ntAlt")
  def makeOperatorNonterminalSeqWithName[L, N, T](c: Context)(
    p: c.Expr[OperatorSequenceBuilder[L, N, T]]
  ): c.Expr[OperatorNonterminal[L, N] & T] = makeCallWithName(c, "OperatorParsers.ntSeq")
  def makeOperatorNonterminalSymWithName[L, N, T](c: Context)(
    p: c.Expr[AbstractOperatorNonterminal[L, N, T]]
  ): c.Expr[OperatorNonterminal[L, N] & T] = makeCallWithName(c, "OperatorParsers.ntSym")
  def makeOperatorNonterminalSeqWithActionWithName[L, N, T](c: Context)(
    p: c.Expr[OperatorSequenceBuilderWithAction[L, N, T]]
  ): c.Expr[OperatorNonterminal[L, N] & T] = makeCallWithName(c, "OperatorParsers.ntSeqWithAction")
  def makeOperatorNonterminalSymWithActionWithName[L, N, T](c: Context)(
    p: c.Expr[OperatorNonterminalWithAction[L, N,T]]
  ): c.Expr[OperatorNonterminal[L, N] & T] = makeCallWithName(c, "OperatorParsers.ntSymWithAction")
}
