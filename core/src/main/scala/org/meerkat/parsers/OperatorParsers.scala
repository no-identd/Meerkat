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

    implicit def obj1[E, N, ValA, ValB](implicit vals: ValA |~| ValB) =
      new CanBuildSequence[E, N,NonPackedNode, NonPackedNode, ValA, ValB] {
        implicit val o = Parsers.obj1[E, N, ValA, ValB](vals)

        type OperatorSequence = OperatorParsers.OperatorSequence[E, N,o.V]

        def sequence(p: AbstractOperatorSequence): OperatorSequence = new OperatorSequence {
          def apply(prec1: Prec, prec2: Prec) = p(prec1, prec2)
          def infix                           = p.infix; def prefix = p.prefix; def postfix = p.postfix
          def assoc                           = p.assoc
        }

        def assoc(p: OperatorSequence, a: Assoc.Assoc): OperatorSequence =
          if ((p.infix && a != Assoc.NON_ASSOC)
              || ((p.infix || p.prefix || p.postfix) && a == Assoc.NON_ASSOC))
            new OperatorSequence {
              def apply(prec1: Prec, prec2: Prec) = p(prec1, prec2)
              def infix                           = p.infix; def prefix = p.prefix; def postfix = p.postfix
              def assoc: Assoc.Assoc              = a
            } else p

        type OperatorSequenceBuilder = OperatorParsers.OperatorSequenceBuilder[E, N,o.V]
        def builderSeq(f: Head[E, N] => OperatorSequence): OperatorSequenceBuilder = new OperatorSequenceBuilder {
          def apply(head: Head[E, N]) = f(head)
        }
      }

    implicit def obj2[E, N, ValA, ValB] = new CanBuildAlternation[E, N,NonPackedNode, NonPackedNode, ValA, ValB] {
      implicit val o = Parsers.obj3[E, N, ValA, ValB]

      type OperatorAlternation = OperatorParsers.OperatorAlternation[E, N, ValB]
      def alternation(f: Prec => o.AlternationBuilder): OperatorAlternation = new OperatorAlternation {
        def apply(prec: Prec) = f(prec)
      }

      type OperatorAlternationBuilder = OperatorParsers.OperatorAlternationBuilder[E, N,ValB]
      def builderAlt(
        f: (Head[E, N], Group) => (Group => OperatorAlternation, Group, Option[Group])
      ): OperatorAlternationBuilder = new OperatorAlternationBuilder {
        def apply(head: Head[E, N], group: Group) = f(head, group)
      }
    }

    implicit def obj3[E, N, Val] = new CanBuildNonterminal[E, N,NonPackedNode, Val] {
      implicit val o1 = Parsers.obj5[E, N, Val]
      implicit val o2 = Parsers.obj2

      type OperatorNonterminal = OperatorParsers.AbstractOperatorNonterminal[E, N,Val]
      def nonterminal(ntName: String, f: Prec => o1.Nonterminal): OperatorNonterminal = new OperatorNonterminal {
        val table: java.util.Map[Prec, o1.Nonterminal] = new HashMap()
        def apply(prec: Prec) =
          if (table.containsKey(prec)) table.get(prec)
          else { val nt = f(prec); table.put(prec, nt); nt }
        def name = ntName; override def toString = ntName
      }
    }
  }

  trait OperatorSequence[E, N,+V] extends ((Prec, Prec) => Parsers.SequenceBuilder[E, N,V]) {
    def infix: Boolean; def prefix: Boolean; def postfix: Boolean; def assoc: Assoc.Assoc
  }

  trait OperatorSequenceWithAction[E, N,+V] extends ((Prec, Prec) => Parsers.SequenceBuilderWithAction[E, N,V]) {
    def infix: Boolean; def prefix: Boolean; def postfix: Boolean; def assoc: Assoc.Assoc
  }

  trait OperatorAlternation[E, N, +V] extends (Prec => Parsers.AlternationBuilder[E, N,V])

  trait AbstractOperatorNonterminal[E, N,+V] extends (Prec => Parsers.AbstractNonterminal[E, N,V]) with EBNFs[E, N, V] {
    import OperatorImplicits._; import AbstractOperatorParser._
    type Abstract[+X] = AbstractOperatorNonterminal[E, N,X]
    def name: String

    def ~[U](p: AbstractOperatorNonterminal[E, N,U])(implicit tuple: V |~| U)                = seqNt(this, p)
    def ~[U](p: Symbol[E, N, U])(implicit tuple: V |~| U)                                     = seqNtSym(this, p)

    def |[U >: V](p: OperatorAlternationBuilder[E, N,U])        = altOpSymOpAlt(this, p)
    def |[U >: V](p: OperatorSequenceBuilder[E, N,U])           = altOpSymOpSeq(this, p)
    def |[U >: V](p: AbstractOperatorNonterminal[E, N,U])       = altOpSym(this, p)
    def |[U >: V](p: OperatorSequenceBuilderWithAction[E, N,U]) = altOpSymOpSeq(this, p)
    def |[U >: V](p: OperatorNonterminalWithAction[E, N, U])     = altOpSym(this, p)

    def |[U >: V](p: AlternationBuilder[E, N,U])        = altOpSymOpAlt(this, altAltOpAlt(p))
    def |[U >: V](p: SequenceBuilder[E, N,U])           = altOpSymOpSeq(this, altSeqOpSeq(p))
    def |[U >: V](p: Symbol[E, N, U])                    = altOpSym(this, altSymOpSym(p))
    def |[U >: V](p: SequenceBuilderWithAction[E, N,U]) = altOpSymOpSeq(this, altSeqOpSeq(p))
    def |[U >: V](p: SymbolWithAction[E, N,U])          = altOpSym(this, altSymOpSym(p))

    def &[U](f: V => U) = new OperatorNonterminalWithAction[E, N,U] {
      def apply(prec: Prec) = AbstractOperatorNonterminal.this(prec) & f
      def name              = AbstractOperatorNonterminal.this.name
    }

    def ^[U](f: String => U)(implicit sub: V <:< NoValue) = new OperatorNonterminalWithAction[E, N,U] {
      def apply(prec: Prec) = AbstractOperatorNonterminal.this(prec) ^ f
      def name              = AbstractOperatorNonterminal.this.name
    }
  }

  trait OperatorNonterminalWithAction[E, N, +V] extends (Prec => Parsers.SymbolWithAction[E, N,V]) {
    import OperatorImplicits._; import AbstractOperatorParser._
    def name: String

    def |[U >: V](p: OperatorAlternationBuilder[E, N,U])        = altOpSymOpAlt(this, p)
    def |[U >: V](p: OperatorSequenceBuilder[E, N,U])           = altOpSymOpSeq(this, p)
    def |[U >: V](p: AbstractOperatorNonterminal[E, N,U])       = altOpSym(this, p)
    def |[U >: V](p: OperatorSequenceBuilderWithAction[E, N,U]) = altOpSymOpSeq(this, p)
    def |[U >: V](p: OperatorNonterminalWithAction[E, N,U])     = altOpSym(this, p)

    def |[U >: V](p: AlternationBuilder[E, N,U])        = altOpSymOpAlt(this, altAltOpAlt(p))
    def |[U >: V](p: SequenceBuilder[E, N,U])           = altOpSymOpSeq(this, altSeqOpSeq(p))
    def |[U >: V](p: Symbol[E, N, U])                    = altOpSym(this, altSymOpSym(p))
    def |[U >: V](p: SequenceBuilderWithAction[E, N,U]) = altOpSymOpSeq(this, altSeqOpSeq(p))
    def |[U >: V](p: SymbolWithAction[E, N,U])          = altOpSym(this, altSymOpSym(p))
  }

  type OperatorNonterminal[E, N] = AbstractOperatorNonterminal[E, N,NoValue]

  trait OperatorSequenceBuilder[E, N,+V] extends (Head[E, N] => OperatorSequence[E, N,V]) {
    import OperatorImplicits._; import AbstractOperatorParser._

    def ~[U](p: AbstractOperatorNonterminal[E, N,U])(implicit tuple: V |~| U) = seqOpSeqNt(this, p)
    def ~[U](p: Symbol[E, N, U])(implicit tuple: V |~| U)                      = seqOpSeqSym(this, p)

    def |[U >: V](p: OperatorAlternationBuilder[E, N,U])        = altOpSeqOpAlt(this, p)
    def |[U >: V](p: OperatorSequenceBuilder[E, N,U])           = altOpSeq(this, p)
    def |[U >: V](p: AbstractOperatorNonterminal[E, N,U])       = altOpSeqOpSym(this, p)
    def |[U >: V](p: OperatorSequenceBuilderWithAction[E, N,U]) = altOpSeq(this, p)
    def |[U >: V](p: OperatorNonterminalWithAction[E, N,U])     = altOpSeqOpSym(this, p)

    def |[U >: V](p: AlternationBuilder[E, N,U])        = altOpSeqOpAlt(this, altAltOpAlt(p))
    def |[U >: V](p: SequenceBuilder[E, N,U])           = altOpSeq(this, altSeqOpSeq(p))
    def |[U >: V](p: Symbol[E, N, U])                    = altOpSeqOpSym(this, altSymOpSym(p))
    def |[U >: V](p: SequenceBuilderWithAction[E, N,U]) = altOpSeq(this, altSeqOpSeq(p))
    def |[U >: V](p: SymbolWithAction[E, N,U])          = altOpSeqOpSym(this, altSymOpSym(p))

    def |>[U >: V](p: OperatorAlternationBuilder[E, N,U])        = greaterOpSeqOpAlt(this, p)
    def |>[U >: V](p: OperatorSequenceBuilder[E, N,U])           = greaterOpSeq(this, p)
    def |>[U >: V](p: OperatorSequenceBuilderWithAction[E, N,U]) = greaterOpSeq(this, p)

    def &[U](f: V => U) = new OperatorSequenceBuilderWithAction[E, N,U] {
      def apply(head: Head[E, N]) = {
        val p = OperatorSequenceBuilder.this(head)
        new OperatorSequenceWithAction[E, N,U] {
          def apply(prec1: Prec, prec2: Prec) = p(prec1, prec2) & f
          def infix                           = p.infix; def prefix = p.prefix; def postfix = p.postfix; def assoc = p.assoc
        }
      }
    }

    def ^[U](f: String => U)(implicit sub: V <:< NoValue) = new OperatorSequenceBuilderWithAction[E, N,U] {
      def apply(head: Head[E, N]) = {
        val p = OperatorSequenceBuilder.this(head)
        new OperatorSequenceWithAction[E, N,U] {
          def apply(prec1: Prec, prec2: Prec) = p(prec1, prec2) ^ f
          def infix                           = p.infix; def prefix = p.prefix; def postfix = p.postfix; def assoc = p.assoc
        }
      }
    }
  }

  trait OperatorSequenceBuilderWithAction[E, N,+V] extends (Head[E, N] => OperatorSequenceWithAction[E, N,V]) {
    import OperatorImplicits._; import AbstractOperatorParser._
    def |[U >: V](p: OperatorAlternationBuilder[E, N,U])        = altOpSeqOpAlt(this, p)
    def |[U >: V](p: OperatorSequenceBuilder[E, N,U])           = altOpSeq(this, p)
    def |[U >: V](p: AbstractOperatorNonterminal[E, N,U])       = altOpSeqOpSym(this, p)
    def |[U >: V](p: OperatorSequenceBuilderWithAction[E, N,U]) = altOpSeq(this, p)
    def |[U >: V](p: OperatorNonterminalWithAction[E, N,U])     = altOpSeqOpSym(this, p)

    def |[U >: V](p: AlternationBuilder[E, N,U])        = altOpSeqOpAlt(this, altAltOpAlt(p))
    def |[U >: V](p: SequenceBuilder[E, N,U])           = altOpSeq(this, altSeqOpSeq(p))
    def |[U >: V](p: Symbol[E, N, U])                    = altOpSeqOpSym(this, altSymOpSym(p))
    def |[U >: V](p: SequenceBuilderWithAction[E, N,U]) = altOpSeq(this, altSeqOpSeq(p))
    def |[U >: V](p: SymbolWithAction[E, N,U])          = altOpSeqOpSym(this, altSymOpSym(p))

    def |>[U >: V](p: OperatorAlternationBuilder[E, N,U])        = greaterOpSeqOpAlt(this, p)
    def |>[U >: V](p: OperatorSequenceBuilder[E, N,U])           = greaterOpSeq(this, p)
    def |>[U >: V](p: OperatorSequenceBuilderWithAction[E, N,U]) = greaterOpSeq(this, p)
  }

  trait OperatorAlternationBuilder[E, N,+V]
      extends ((Head[E, N], Group) => (Group => OperatorAlternation[E, N, V], Group, Option[Group])) {
    import OperatorImplicits._; import AbstractOperatorParser._
    def |[U >: V](p: OperatorAlternationBuilder[E, N,U])        = altOpAlt(this, p)
    def |[U >: V](p: OperatorSequenceBuilder[E, N,U])           = altOpAltOpSeq(this, p)
    def |[U >: V](p: AbstractOperatorNonterminal[E, N,U])       = altOpAltOpSym(this, p)
    def |[U >: V](p: OperatorSequenceBuilderWithAction[E, N,U]) = altOpAltOpSeq(this, p)
    def |[U >: V](p: OperatorNonterminalWithAction[E, N,U])     = altOpAltOpSym(this, p)

    def |[U >: V](p: AlternationBuilder[E, N,U])        = altOpAlt(this, altAltOpAlt(p))
    def |[U >: V](p: SequenceBuilder[E, N,U])           = altOpAltOpSeq(this, altSeqOpSeq(p))
    def |[U >: V](p: Symbol[E, N, U])                    = altOpAltOpSym(this, altSymOpSym(p))
    def |[U >: V](p: SequenceBuilderWithAction[E, N,U]) = altOpAltOpSeq(this, altSeqOpSeq(p))
    def |[U >: V](p: SymbolWithAction[E, N,U])          = altOpAltOpSym(this, altSymOpSym(p))

    def |>[U >: V](p: OperatorAlternationBuilder[E, N,U])        = greaterOpAlt(this, p)
    def |>[U >: V](p: OperatorSequenceBuilder[E, N,U])           = greaterOpAltOpSeq(this, p)
    def |>[U >: V](p: OperatorSequenceBuilderWithAction[E, N,U]) = greaterOpAltOpSeq(this, p)
  }

  implicit class ParsersSeqOps[E, N,V](p: Parsers.Symbol[E, N, V]) {
    import OperatorImplicits._; import AbstractOperatorParser._
    def ~[U](q: AbstractOperatorNonterminal[E, N,U])(implicit tuple: V |~| U)                = seqSymNt(p, q)

    def |[U >: V](q: OperatorAlternationBuilder[E, N,U])        = altOpSymOpAlt(altSymOpSym(p), q)
    def |[U >: V](q: OperatorSequenceBuilder[E, N,U])           = altOpSymOpSeq(altSymOpSym(p), q)
    def |[U >: V](q: AbstractOperatorNonterminal[E, N,U])       = altOpSym(altSymOpSym(p), q)
    def |[U >: V](q: OperatorSequenceBuilderWithAction[E, N,U]) = altOpSymOpSeq(altSymOpSym(p), q)
    def |[U >: V](q: OperatorNonterminalWithAction[E, N,U])     = altOpSym(altSymOpSym(p), q)
  }

  implicit class ParsersAltOps2[E, N,V](p: Parsers.AlternationBuilder[E, N,V]) {
    import OperatorImplicits._; import AbstractOperatorParser._
    def |[U >: V](q: OperatorAlternationBuilder[E, N,U])        = altOpAlt(altAltOpAlt(p), q)
    def |[U >: V](q: OperatorSequenceBuilder[E, N,U])           = altOpAltOpSeq(altAltOpAlt(p), q)
    def |[U >: V](q: AbstractOperatorNonterminal[E, N,U])       = altOpAltOpSym(altAltOpAlt(p), q)
    def |[U >: V](q: OperatorSequenceBuilderWithAction[E, N,U]) = altOpAltOpSeq(altAltOpAlt(p), q)
    def |[U >: V](q: OperatorNonterminalWithAction[E, N,U])     = altOpAltOpSym(altAltOpAlt(p), q)
  }

  implicit class ParsersAltOps1[E, N, +V](p: Parsers.SequenceBuilder[E, N,V]) {
    import OperatorImplicits._; import AbstractOperatorParser._
    def |[U >: V](q: OperatorAlternationBuilder[E, N,U])        = altOpSeqOpAlt(altSeqOpSeq(p), q)
    def |[U >: V](q: OperatorSequenceBuilder[E, N,U])           = altOpSeq(altSeqOpSeq(p), q)
    def |[U >: V](q: AbstractOperatorNonterminal[E, N,U])       = altOpSeqOpSym(altSeqOpSeq(p), q)
    def |[U >: V](q: OperatorSequenceBuilderWithAction[E, N,U]) = altOpSeq(altSeqOpSeq(p), q)
    def |[U >: V](q: OperatorNonterminalWithAction[E, N,U])     = altOpSeqOpSym(altSeqOpSeq(p), q)
  }

//  implicit class StringSeqOps[E, N](term: E) {
//    import OperatorImplicits._; import AbstractOperatorParser._
//    val p =
//      Parsers.toTerminal(term)
//    def ~[ U](q: AbstractOperatorNonterminal[E, N,U])
//             (implicit tuple: NoValue |~| U)
//    = seqSymNt(p, q)
//  }

//  implicit class StringAltOps(term: String) { import OperatorImplicits._; import AbstractOperatorParser._
//    val p: Symbol { type Value = NoValue } = term
//
//    def | (q: OperatorAlternationBuilder[E, N,NoValue]) = altOpSymOpAlt(altSymOpSym(p), q)
//    def | (q: OperatorSequenceBuilder[E, N,NoValue]) = altOpSymOpSeq(altSymOpSym(p), q)
//    def | (q: AbstractOperatorNonterminal[E, N,NoValue]) = altOpSym(altSymOpSym(p), q)
//    def | (q: OperatorSequenceBuilderWithAction[E, N,NoValue]) = altOpSymOpSeq(altSymOpSym(p), q)
//    def | (q: OperatorNonterminalWithAction[E, N,NoValue]) = altOpSym(altSymOpSym(p), q)
//
//    def ^ [U](f: String => U)(implicit sub: p.Value <:< NoValue) = p ^ f
//  }

  def left[E, N, V](p: OperatorSequenceBuilder[E, N,V]): OperatorSequenceBuilder[E, N,V] = {
    import OperatorImplicits._
    val o = obj1[E, N, V, V](new |~|[V, V] { type R = V })
    o.builderSeq(head => o.assoc(p(head), Assoc.LEFT))
  }

  def right[E, N, V](p: OperatorSequenceBuilder[E, N,V]): OperatorSequenceBuilder[E, N,V] = {
    import OperatorImplicits._
    val o = obj1[E, N, V, V](new |~|[V, V] { type R = V })
    o.builderSeq(head => o.assoc(p(head), Assoc.RIGHT))
  }

  def non_assoc[E, N, V](p: OperatorSequenceBuilder[E, N,V]): OperatorSequenceBuilder[E, N,V] = {
    import OperatorImplicits._
    val o = obj1[E, N, V, V](new |~|[V, V] { type R = V })
    o.builderSeq(head => o.assoc(p(head), Assoc.NON_ASSOC))
  }

  def left[E, N, Val](p: OperatorAlternationBuilder[E, N,Val]): OperatorAlternationBuilder[E, N,Val] = {
    import OperatorImplicits._; import AbstractOperatorParser.assocAlt
    assocAlt(obj2[E, N, Val, Val])(p, Assoc.LEFT)
  }

  def right[E, N, Val](p: OperatorAlternationBuilder[E, N,Val]): OperatorAlternationBuilder[E, N,Val] = {
    import OperatorImplicits._; import AbstractOperatorParser.assocAlt
    assocAlt(obj2[E, N, Val, Val])(p, Assoc.RIGHT)
  }

  def non_assoc[E, N, Val](p: OperatorAlternationBuilder[E, N,Val]): OperatorAlternationBuilder[E, N,Val] = {
    import OperatorImplicits._; import AbstractOperatorParser.assocAlt
    assocAlt(obj2[E, N, Val, Val])(p, Assoc.NON_ASSOC)
  }

  def ntAlt[E, N, Val](name: String, p: => OperatorAlternationBuilder[E, N,Val]): AbstractOperatorNonterminal[E, N,Val] = {
    import OperatorImplicits._; import AbstractOperatorParser.nonterminalAlt
    nonterminalAlt(name, p)
  }
  def ntSeq[E, N, Val](name: String, p: => OperatorSequenceBuilder[E, N,Val]): AbstractOperatorNonterminal[E, N,Val] = {
    import OperatorImplicits._; import AbstractOperatorParser.nonterminalSeq
    nonterminalSeq(name, p)
  }
  def ntSym[E, N, Val](name: String, p: => AbstractOperatorNonterminal[E, N,Val]): AbstractOperatorNonterminal[E, N,Val] = {
    import OperatorImplicits._; import AbstractOperatorParser.nonterminalSym
    nonterminalSym(name, p)
  }
  def ntSeqWithAction[E, N, Val](name: String,
                           p: => OperatorSequenceBuilderWithAction[E, N,Val]): AbstractOperatorNonterminal[E, N,Val] = {
    import OperatorImplicits._; import AbstractOperatorParser.nonterminalSeq
    nonterminalSeq(name, p)
  }
  def ntSymWithAction[E, N, Val](name: String, p: => OperatorNonterminalWithAction[E, N,Val]): AbstractOperatorNonterminal[E, N,Val] = {
    import OperatorImplicits._; import AbstractOperatorParser.nonterminalSym
    nonterminalSym(name, p)
  }

  // TODO: the same as in Parsers.scala?
  trait EBNFs[E, N, +V] { self: AbstractOperatorNonterminal[E, N,V] =>
//    var star: Option[AbstractOperatorNonterminal[E, N,_]] = None
//    def *(implicit ebnf: EBNF[V]): AbstractOperatorNonterminal[E, N,ebnf.OptOrSeq] =
//      star
//        .getOrElse({
//          val p = new AbstractOperatorNonterminal[E, N,ebnf.OptOrSeq] {
//            def apply(prec: Prec) = self($).*(ebnf); def name = self.name + "*"
//          }
//          star = Option(p); p
//        })
//        .asInstanceOf[AbstractOperatorNonterminal[E, N,ebnf.OptOrSeq]]
//
//    val star_sep: mutable.Map[E, AbstractOperatorNonterminal[E, N,_]] =
//      mutable.HashMap()
//    def *(sep: E)(implicit ebnf: EBNF[V]): AbstractOperatorNonterminal[E, N,ebnf.OptOrSeq] =
//      star_sep
//        .getOrElseUpdate(sep, new AbstractOperatorNonterminal[E, N,ebnf.OptOrSeq] {
//          def apply(prec: Prec) = self($).*(sep)(ebnf); def name = s"{${self.name} $sep}*"
//        })
//        .asInstanceOf[AbstractOperatorNonterminal[E, N,ebnf.OptOrSeq]]
//
//    var plus: Option[AbstractOperatorNonterminal[E, N,_]] = None
//    def +(implicit ebnf: EBNF[V]): AbstractOperatorNonterminal[E, N,ebnf.OptOrSeq] =
//      plus
//        .getOrElse({
//          val p = new AbstractOperatorNonterminal[E, N,ebnf.OptOrSeq] {
//            def apply(prec: Prec) = self($).+; def name = self.name + "+"
//          }
//          plus = Option(p); p
//        })
//        .asInstanceOf[AbstractOperatorNonterminal[E, N,ebnf.OptOrSeq]]
//
//    val plus_sep: mutable.Map[E, AbstractOperatorNonterminal[E, N,_]] =
//      mutable.HashMap()
//    def +(sep: E)(implicit ebnf: EBNF[V]): AbstractOperatorNonterminal[E, N,ebnf.OptOrSeq] =
//      plus_sep
//        .getOrElseUpdate(sep, new AbstractOperatorNonterminal[E, N,ebnf.OptOrSeq] {
//          def apply(prec: Prec) = self($).+(sep)(ebnf); def name = s"{${self.name} $sep}+"
//        })
//        .asInstanceOf[AbstractOperatorNonterminal[E, N,ebnf.OptOrSeq]]
  }
}
