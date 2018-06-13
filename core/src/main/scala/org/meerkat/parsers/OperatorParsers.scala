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

package org.meerkat.parsers

import org.meerkat.sppf.NonPackedNode
import java.util.HashMap
import org.meerkat.util.visualization._
import scala.collection.mutable

object OperatorParsers {

  import AbstractOperatorParsers._
  import Parsers._

  object OperatorImplicits {

    implicit def obj1[L, N, ValA, ValB](implicit vals: ValA |~| ValB) =
      new CanBuildSequence[L, N, NonPackedNode, NonPackedNode, ValA, ValB] {
        implicit val o = Parsers.obj1[L, N, ValA, ValB](vals)

        type OperatorSequence = OperatorParsers.OperatorSequence[L, N, o.V]

        def sequence(p: AbstractOperatorSequence): OperatorSequence =
          new OperatorSequence {
            def apply(prec1: Prec, prec2: Prec) = p(prec1, prec2)
            def infix                           = p.infix; def prefix = p.prefix; def postfix = p.postfix
            def assoc                           = p.assoc
          }

        def assoc(p: OperatorSequence, a: Assoc.Assoc): OperatorSequence =
          if ((p.infix && a != Assoc.NON_ASSOC)
              || ((p.infix || p.prefix || p.postfix) && a == Assoc.NON_ASSOC))
            new OperatorSequence {
              def apply(prec1: Prec, prec2: Prec) = p(prec1, prec2)
              def infix                           = p.infix; def prefix = p.prefix;
              def postfix                         = p.postfix
              def assoc: Assoc.Assoc              = a
            } else p

        type OperatorSequenceBuilder =
          OperatorParsers.OperatorSequenceBuilder[L, N, o.V]
        def builderSeq(
            f: Head[L, N] => OperatorSequence): OperatorSequenceBuilder =
          new OperatorSequenceBuilder {
            def apply(head: Head[L, N]) = f(head)
          }
      }

    implicit def obj2[L, N, ValA, ValB] =
      new CanBuildAlternation[L, N, NonPackedNode, NonPackedNode, ValA, ValB] {
        implicit val o = Parsers.obj3[L, N, ValA, ValB]

        type OperatorAlternation =
          OperatorParsers.OperatorAlternation[L, N, ValB]
        def alternation(f: Prec => o.AlternationBuilder): OperatorAlternation =
          new OperatorAlternation {
            def apply(prec: Prec) = f(prec)
          }

        type OperatorAlternationBuilder =
          OperatorParsers.OperatorAlternationBuilder[L, N, ValB]
        def builderAlt(
            f: (Head[L, N], Group) => (Group => OperatorAlternation,
                                       Group,
                                       Option[Group])
        ): OperatorAlternationBuilder = new OperatorAlternationBuilder {
          def apply(head: Head[L, N], group: Group) = f(head, group)
        }
      }

    implicit def obj3[L, N, Val] =
      new CanBuildNonterminal[L, N, NonPackedNode, Val] {
        implicit val o1 = Parsers.obj5[L, N, Val]
        implicit val o2 = Parsers.obj2

        type OperatorNonterminal =
          OperatorParsers.AbstractOperatorNonterminal[L, N, Val]
        def nonterminal(ntName: String,
                        f: Prec => o1.Nonterminal): OperatorNonterminal =
          new OperatorNonterminal {
            val table: java.util.Map[Prec, o1.Nonterminal] = new HashMap()
            def apply(prec: Prec) =
              if (table.containsKey(prec)) table.get(prec)
              else { val nt = f(prec); table.put(prec, nt); nt }
            def name = ntName; override def toString = ntName
          }
      }
  }

  trait OperatorSequence[L, N, +V]
      extends ((Prec, Prec) => Parsers.SequenceBuilder[L, N, V]) {
    def infix: Boolean; def prefix: Boolean; def postfix: Boolean;
    def assoc: Assoc.Assoc
  }

  trait OperatorSequenceWithAction[L, N, +V]
      extends ((Prec, Prec) => Parsers.SequenceBuilder[L, N, V]) {
    def infix: Boolean; def prefix: Boolean; def postfix: Boolean;
    def assoc: Assoc.Assoc
  }

  trait OperatorAlternation[L, N, +V]
      extends (Prec => Parsers.AlternationBuilder[L, N, V])

  trait AbstractOperatorNonterminal[L, N, +V]
      extends (Prec => Parsers.AbstractNonterminal[L, N, V])
      with EBNFs[L, N, V] {
    import OperatorImplicits._; import AbstractOperatorParser._
    type Abstract[+X] = AbstractOperatorNonterminal[L, N, X]
    def name: String

    def ~[U](p: AbstractOperatorNonterminal[L, N, U])(implicit tuple: V |~| U) =
      seqNt(this, p)
    def ~[U](p: Symbol[L, N, U])(implicit tuple: V |~| U) = seqNtSym(this, p)

    def |[U >: V](p: OperatorAlternationBuilder[L, N, U]) =
      altOpSymOpAlt(this, p)
    def |[U >: V](p: OperatorSequenceBuilder[L, N, U])     = altOpSymOpSeq(this, p)
    def |[U >: V](p: AbstractOperatorNonterminal[L, N, U]) = altOpSym(this, p)
    def |[U >: V](p: OperatorSequenceBuilderWithAction[L, N, U]) =
      altOpSymOpSeq(this, p)
    def |[U >: V](p: OperatorNonterminalWithAction[L, N, U]) = altOpSym(this, p)

    def |[U >: V](p: AlternationBuilder[L, N, U]) =
      altOpSymOpAlt(this, altAltOpAlt(p))
    def |[U >: V](p: SequenceBuilder[L, N, U]) =
      altOpSymOpSeq(this, altSeqOpSeq(p))
    def |[U >: V](p: Symbol[L, N, U]) = altOpSym(this, altSymOpSym(p))
    def |[U >: V](p: SequenceBuilderWithAction[L, N, U]) =
      altOpSymOpSeq(this, altSeqOpSeq(p))
    def |[U >: V](p: SymbolWithAction[L, N, U]) = altOpSym(this, altSymOpSym(p))

    def &[U](f: V => U) = new OperatorNonterminalWithAction[L, N, U] {
      def apply(prec: Prec) = AbstractOperatorNonterminal.this(prec) & f
      def name              = AbstractOperatorNonterminal.this.name
    }

    /*
    def ^[U](f: L => U)(implicit sub: V <:< NoValue) = new OperatorNonterminalWithAction[L, N,U] {
      def apply(prec: Prec) = AbstractOperatorNonterminal.this(prec) ^ f
      def name              = AbstractOperatorNonterminal.this.name
    }*/
  }

  trait OperatorNonterminalWithAction[L, N, +V]
      extends (Prec => Parsers.SymbolWithAction[L, N, V]) {
    import OperatorImplicits._; import AbstractOperatorParser._
    def name: String

    def |[U >: V](p: OperatorAlternationBuilder[L, N, U]) =
      altOpSymOpAlt(this, p)
    def |[U >: V](p: OperatorSequenceBuilder[L, N, U])     = altOpSymOpSeq(this, p)
    def |[U >: V](p: AbstractOperatorNonterminal[L, N, U]) = altOpSym(this, p)
    def |[U >: V](p: OperatorSequenceBuilderWithAction[L, N, U]) =
      altOpSymOpSeq(this, p)
    def |[U >: V](p: OperatorNonterminalWithAction[L, N, U]) = altOpSym(this, p)

    def |[U >: V](p: AlternationBuilder[L, N, U]) =
      altOpSymOpAlt(this, altAltOpAlt(p))
    def |[U >: V](p: SequenceBuilder[L, N, U]) =
      altOpSymOpSeq(this, altSeqOpSeq(p))
    def |[U >: V](p: Symbol[L, N, U]) = altOpSym(this, altSymOpSym(p))
    def |[U >: V](p: SequenceBuilderWithAction[L, N, U]) =
      altOpSymOpSeq(this, altSeqOpSeq(p))
    def |[U >: V](p: SymbolWithAction[L, N, U]) = altOpSym(this, altSymOpSym(p))
  }

  type OperatorNonterminal[L, N] = AbstractOperatorNonterminal[L, N, NoValue]

  trait OperatorSequenceBuilder[L, N, +V]
      extends (Head[L, N] => OperatorSequence[L, N, V]) {
    import OperatorImplicits._; import AbstractOperatorParser._

    def ~[U](p: AbstractOperatorNonterminal[L, N, U])(implicit tuple: V |~| U) =
      seqOpSeqNt(this, p)
    def ~[U](p: Symbol[L, N, U])(implicit tuple: V |~| U) = seqOpSeqSym(this, p)

    def |[U >: V](p: OperatorAlternationBuilder[L, N, U]) =
      altOpSeqOpAlt(this, p)
    def |[U >: V](p: OperatorSequenceBuilder[L, N, U]) = altOpSeq(this, p)
    def |[U >: V](p: AbstractOperatorNonterminal[L, N, U]) =
      altOpSeqOpSym(this, p)
    def |[U >: V](p: OperatorSequenceBuilderWithAction[L, N, U]) =
      altOpSeq(this, p)
    def |[U >: V](p: OperatorNonterminalWithAction[L, N, U]) =
      altOpSeqOpSym(this, p)

    def |[U >: V](p: AlternationBuilder[L, N, U]) =
      altOpSeqOpAlt(this, altAltOpAlt(p))
    def |[U >: V](p: SequenceBuilder[L, N, U]) = altOpSeq(this, altSeqOpSeq(p))
    def |[U >: V](p: Symbol[L, N, U])          = altOpSeqOpSym(this, altSymOpSym(p))
    def |[U >: V](p: SequenceBuilderWithAction[L, N, U]) =
      altOpSeq(this, altSeqOpSeq(p))
    def |[U >: V](p: SymbolWithAction[L, N, U]) =
      altOpSeqOpSym(this, altSymOpSym(p))

    def |>[U >: V](p: OperatorAlternationBuilder[L, N, U]) =
      greaterOpSeqOpAlt(this, p)
    def |>[U >: V](p: OperatorSequenceBuilder[L, N, U]) = greaterOpSeq(this, p)
    def |>[U >: V](p: OperatorSequenceBuilderWithAction[L, N, U]) =
      greaterOpSeq(this, p)

    def &[U](f: V => U) = new OperatorSequenceBuilderWithAction[L, N, U] {
      def apply(head: Head[L, N]) = {
        val p = OperatorSequenceBuilder.this(head)
        new OperatorSequenceWithAction[L, N, U] {
          def apply(prec1: Prec, prec2: Prec) = p(prec1, prec2) & f
          def infix                           = p.infix; def prefix = p.prefix; def postfix = p.postfix;
          def assoc                           = p.assoc
        }
      }
    }

    def ^[U](f: L => U)(implicit sub: V <:< NoValue) =
      new OperatorSequenceBuilderWithAction[L, N, U] {
        def apply(head: Head[L, N]) = {
          val p = OperatorSequenceBuilder.this(head)
          new OperatorSequenceWithAction[L, N, U] {
            def apply(prec1: Prec, prec2: Prec) = p(prec1, prec2) ^ f
            def infix                           = p.infix; def prefix = p.prefix; def postfix = p.postfix;
            def assoc                           = p.assoc
          }
        }
      }
  }

  trait OperatorSequenceBuilderWithAction[L, N, +V]
      extends (Head[L, N] => OperatorSequenceWithAction[L, N, V]) {
    import OperatorImplicits._; import AbstractOperatorParser._
    def |[U >: V](p: OperatorAlternationBuilder[L, N, U]) =
      altOpSeqOpAlt(this, p)
    def |[U >: V](p: OperatorSequenceBuilder[L, N, U]) = altOpSeq(this, p)
    def |[U >: V](p: AbstractOperatorNonterminal[L, N, U]) =
      altOpSeqOpSym(this, p)
    def |[U >: V](p: OperatorSequenceBuilderWithAction[L, N, U]) =
      altOpSeq(this, p)
    def |[U >: V](p: OperatorNonterminalWithAction[L, N, U]) =
      altOpSeqOpSym(this, p)

    def |[U >: V](p: AlternationBuilder[L, N, U]) =
      altOpSeqOpAlt(this, altAltOpAlt(p))
    def |[U >: V](p: SequenceBuilder[L, N, U]) = altOpSeq(this, altSeqOpSeq(p))
    def |[U >: V](p: Symbol[L, N, U])          = altOpSeqOpSym(this, altSymOpSym(p))
    def |[U >: V](p: SequenceBuilderWithAction[L, N, U]) =
      altOpSeq(this, altSeqOpSeq(p))
    def |[U >: V](p: SymbolWithAction[L, N, U]) =
      altOpSeqOpSym(this, altSymOpSym(p))

    def |>[U >: V](p: OperatorAlternationBuilder[L, N, U]) =
      greaterOpSeqOpAlt(this, p)
    def |>[U >: V](p: OperatorSequenceBuilder[L, N, U]) = greaterOpSeq(this, p)
    def |>[U >: V](p: OperatorSequenceBuilderWithAction[L, N, U]) =
      greaterOpSeq(this, p)
  }

  trait OperatorAlternationBuilder[L, N, +V]
      extends ((Head[L, N], Group) => (Group => OperatorAlternation[L, N, V],
                                       Group,
                                       Option[Group])) {
    import OperatorImplicits._; import AbstractOperatorParser._
    def |[U >: V](p: OperatorAlternationBuilder[L, N, U]) = altOpAlt(this, p)
    def |[U >: V](p: OperatorSequenceBuilder[L, N, U])    = altOpAltOpSeq(this, p)
    def |[U >: V](p: AbstractOperatorNonterminal[L, N, U]) =
      altOpAltOpSym(this, p)
    def |[U >: V](p: OperatorSequenceBuilderWithAction[L, N, U]) =
      altOpAltOpSeq(this, p)
    def |[U >: V](p: OperatorNonterminalWithAction[L, N, U]) =
      altOpAltOpSym(this, p)

    def |[U >: V](p: AlternationBuilder[L, N, U]) =
      altOpAlt(this, altAltOpAlt(p))
    def |[U >: V](p: SequenceBuilder[L, N, U]) =
      altOpAltOpSeq(this, altSeqOpSeq(p))
    def |[U >: V](p: Symbol[L, N, U]) = altOpAltOpSym(this, altSymOpSym(p))
    def |[U >: V](p: SequenceBuilderWithAction[L, N, U]) =
      altOpAltOpSeq(this, altSeqOpSeq(p))
    def |[U >: V](p: SymbolWithAction[L, N, U]) =
      altOpAltOpSym(this, altSymOpSym(p))

    def |>[U >: V](p: OperatorAlternationBuilder[L, N, U]) =
      greaterOpAlt(this, p)
    def |>[U >: V](p: OperatorSequenceBuilder[L, N, U]) =
      greaterOpAltOpSeq(this, p)
    def |>[U >: V](p: OperatorSequenceBuilderWithAction[L, N, U]) =
      greaterOpAltOpSeq(this, p)
  }

  implicit class ParsersSeqOps[L, N, V](p: Parsers.Symbol[L, N, V]) {
    import OperatorImplicits._; import AbstractOperatorParser._
    def ~[U](q: AbstractOperatorNonterminal[L, N, U])(implicit tuple: V |~| U) =
      seqSymNt(p, q)

    def |[U >: V](q: OperatorAlternationBuilder[L, N, U]) =
      altOpSymOpAlt(altSymOpSym(p), q)
    def |[U >: V](q: OperatorSequenceBuilder[L, N, U]) =
      altOpSymOpSeq(altSymOpSym(p), q)
    def |[U >: V](q: AbstractOperatorNonterminal[L, N, U]) =
      altOpSym(altSymOpSym(p), q)
    def |[U >: V](q: OperatorSequenceBuilderWithAction[L, N, U]) =
      altOpSymOpSeq(altSymOpSym(p), q)
    def |[U >: V](q: OperatorNonterminalWithAction[L, N, U]) =
      altOpSym(altSymOpSym(p), q)
  }

  implicit class ParsersAltOps2[L, N, V](
      p: Parsers.AlternationBuilder[L, N, V]) {
    import OperatorImplicits._; import AbstractOperatorParser._
    def |[U >: V](q: OperatorAlternationBuilder[L, N, U]) =
      altOpAlt(altAltOpAlt(p), q)
    def |[U >: V](q: OperatorSequenceBuilder[L, N, U]) =
      altOpAltOpSeq(altAltOpAlt(p), q)
    def |[U >: V](q: AbstractOperatorNonterminal[L, N, U]) =
      altOpAltOpSym(altAltOpAlt(p), q)
    def |[U >: V](q: OperatorSequenceBuilderWithAction[L, N, U]) =
      altOpAltOpSeq(altAltOpAlt(p), q)
    def |[U >: V](q: OperatorNonterminalWithAction[L, N, U]) =
      altOpAltOpSym(altAltOpAlt(p), q)
  }

  implicit class ParsersAltOps1[L, N, +V](p: Parsers.SequenceBuilder[L, N, V]) {
    import OperatorImplicits._; import AbstractOperatorParser._
    def |[U >: V](q: OperatorAlternationBuilder[L, N, U]) =
      altOpSeqOpAlt(altSeqOpSeq(p), q)
    def |[U >: V](q: OperatorSequenceBuilder[L, N, U]) =
      altOpSeq(altSeqOpSeq(p), q)
    def |[U >: V](q: AbstractOperatorNonterminal[L, N, U]) =
      altOpSeqOpSym(altSeqOpSeq(p), q)
    def |[U >: V](q: OperatorSequenceBuilderWithAction[L, N, U]) =
      altOpSeq(altSeqOpSeq(p), q)
    def |[U >: V](q: OperatorNonterminalWithAction[L, N, U]) =
      altOpSeqOpSym(altSeqOpSeq(p), q)
  }

//  implicit class StringSeqOps[L, N](term: E) {
//    import OperatorImplicits._; import AbstractOperatorParser._
//    val p =
//      Parsers.toTerminal(term)
//    def ~[ U](q: AbstractOperatorNonterminal[L, N,U])
//             (implicit tuple: NoValue |~| U)
//    = seqSymNt(p, q)
//  }

//  implicit class StringAltOps(term: String) { import OperatorImplicits._; import AbstractOperatorParser._
//    val p: Symbol { type Value = NoValue } = term
//
//    def | (q: OperatorAlternationBuilder[L, N,NoValue]) = altOpSymOpAlt(altSymOpSym(p), q)
//    def | (q: OperatorSequenceBuilder[L, N,NoValue]) = altOpSymOpSeq(altSymOpSym(p), q)
//    def | (q: AbstractOperatorNonterminal[L, N,NoValue]) = altOpSym(altSymOpSym(p), q)
//    def | (q: OperatorSequenceBuilderWithAction[L, N,NoValue]) = altOpSymOpSeq(altSymOpSym(p), q)
//    def | (q: OperatorNonterminalWithAction[L, N,NoValue]) = altOpSym(altSymOpSym(p), q)
//
//    def ^ [U](f: String => U)(implicit sub: p.Value <:< NoValue) = p ^ f
//  }

  def left[L, N, V](
      p: OperatorSequenceBuilder[L, N, V]): OperatorSequenceBuilder[L, N, V] = {
    import OperatorImplicits._
    val o = obj1[L, N, V, V](new |~|[V, V] { type R = V })
    o.builderSeq(head => o.assoc(p(head), Assoc.LEFT))
  }

  def right[L, N, V](
      p: OperatorSequenceBuilder[L, N, V]): OperatorSequenceBuilder[L, N, V] = {
    import OperatorImplicits._
    val o = obj1[L, N, V, V](new |~|[V, V] { type R = V })
    o.builderSeq(head => o.assoc(p(head), Assoc.RIGHT))
  }

  def non_assoc[L, N, V](
      p: OperatorSequenceBuilder[L, N, V]): OperatorSequenceBuilder[L, N, V] = {
    import OperatorImplicits._
    val o = obj1[L, N, V, V](new |~|[V, V] { type R = V })
    o.builderSeq(head => o.assoc(p(head), Assoc.NON_ASSOC))
  }

  def left[L, N, Val](p: OperatorAlternationBuilder[L, N, Val])
    : OperatorAlternationBuilder[L, N, Val] = {
    import OperatorImplicits._; import AbstractOperatorParser.assocAlt
    assocAlt(obj2[L, N, Val, Val])(p, Assoc.LEFT)
  }

  def right[L, N, Val](p: OperatorAlternationBuilder[L, N, Val])
    : OperatorAlternationBuilder[L, N, Val] = {
    import OperatorImplicits._; import AbstractOperatorParser.assocAlt
    assocAlt(obj2[L, N, Val, Val])(p, Assoc.RIGHT)
  }

  def non_assoc[L, N, Val](p: OperatorAlternationBuilder[L, N, Val])
    : OperatorAlternationBuilder[L, N, Val] = {
    import OperatorImplicits._; import AbstractOperatorParser.assocAlt
    assocAlt(obj2[L, N, Val, Val])(p, Assoc.NON_ASSOC)
  }

  def ntAlt[L, N, Val](name: String,
                       p: => OperatorAlternationBuilder[L, N, Val])
    : AbstractOperatorNonterminal[L, N, Val] = {
    import OperatorImplicits._; import AbstractOperatorParser.nonterminalAlt
    nonterminalAlt(name, p)
  }
  def ntSeq[L, N, Val](name: String, p: => OperatorSequenceBuilder[L, N, Val])
    : AbstractOperatorNonterminal[L, N, Val] = {
    import OperatorImplicits._; import AbstractOperatorParser.nonterminalSeq
    nonterminalSeq(name, p)
  }
  def ntSym[L, N, Val](name: String,
                       p: => AbstractOperatorNonterminal[L, N, Val])
    : AbstractOperatorNonterminal[L, N, Val] = {
    import OperatorImplicits._; import AbstractOperatorParser.nonterminalSym
    nonterminalSym(name, p)
  }
  def ntSeqWithAction[L, N, Val](
      name: String,
      p: => OperatorSequenceBuilderWithAction[L, N, Val])
    : AbstractOperatorNonterminal[L, N, Val] = {
    import OperatorImplicits._; import AbstractOperatorParser.nonterminalSeq
    nonterminalSeq(name, p)
  }
  def ntSymWithAction[L, N, Val](name: String,
                                 p: => OperatorNonterminalWithAction[L, N, Val])
    : AbstractOperatorNonterminal[L, N, Val] = {
    import OperatorImplicits._; import AbstractOperatorParser.nonterminalSym
    nonterminalSym(name, p)
  }

  // TODO: the same as in Parsers.scala?
  trait EBNFs[L, N, +V] { self: AbstractOperatorNonterminal[L, N, V] =>
//    var star: Option[AbstractOperatorNonterminal[L, N,_]] = None
//    def *(implicit ebnf: EBNF[V]): AbstractOperatorNonterminal[L, N,ebnf.OptOrSeq] =
//      star
//        .getOrElse({
//          val p = new AbstractOperatorNonterminal[L, N,ebnf.OptOrSeq] {
//            def apply(prec: Prec) = self($).*(ebnf); def name = self.name + "*"
//          }
//          star = Option(p); p
//        })
//        .asInstanceOf[AbstractOperatorNonterminal[L, N,ebnf.OptOrSeq]]
//
//    val star_sep: mutable.Map[E, AbstractOperatorNonterminal[L, N,_]] =
//      mutable.HashMap()
//    def *(sep: E)(implicit ebnf: EBNF[V]): AbstractOperatorNonterminal[L, N,ebnf.OptOrSeq] =
//      star_sep
//        .getOrElseUpdate(sep, new AbstractOperatorNonterminal[L, N,ebnf.OptOrSeq] {
//          def apply(prec: Prec) = self($).*(sep)(ebnf); def name = s"{${self.name} $sep}*"
//        })
//        .asInstanceOf[AbstractOperatorNonterminal[L, N,ebnf.OptOrSeq]]
//
//    var plus: Option[AbstractOperatorNonterminal[L, N,_]] = None
//    def +(implicit ebnf: EBNF[V]): AbstractOperatorNonterminal[L, N,ebnf.OptOrSeq] =
//      plus
//        .getOrElse({
//          val p = new AbstractOperatorNonterminal[L, N,ebnf.OptOrSeq] {
//            def apply(prec: Prec) = self($).+; def name = self.name + "+"
//          }
//          plus = Option(p); p
//        })
//        .asInstanceOf[AbstractOperatorNonterminal[L, N,ebnf.OptOrSeq]]
//
//    val plus_sep: mutable.Map[E, AbstractOperatorNonterminal[L, N,_]] =
//      mutable.HashMap()
//    def +(sep: E)(implicit ebnf: EBNF[V]): AbstractOperatorNonterminal[L, N,ebnf.OptOrSeq] =
//      plus_sep
//        .getOrElseUpdate(sep, new AbstractOperatorNonterminal[L, N,ebnf.OptOrSeq] {
//          def apply(prec: Prec) = self($).+(sep)(ebnf); def name = s"{${self.name} $sep}+"
//        })
//        .asInstanceOf[AbstractOperatorNonterminal[L, N,ebnf.OptOrSeq]]
  }
}
