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

import org.meerkat.input.Input
import org.meerkat.sppf.SPPFLookup

import scala.reflect.ClassTag
import org.meerkat.sppf.Slot
import org.meerkat.tree.NonterminalSymbol
import org.meerkat.tree.EdgeSymbol

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.mutable
import scala.language.higherKinds

trait MonadPlus[+T, M[+F] <: MonadPlus[F, M]] {
  def map[U](f: T => U)(implicit m: Memoizable[T]): M[U]
  def flatMap[U](f: T => M[U])(implicit m: Memoizable[T]): M[U]
  def orElse[U >: T](r: => M[U]): M[U]
  def filter(pred: T => Boolean): M[T]

  // Specialization only for optimization purposes
  def smap[U](f: T => U): M[U]
}

trait AbstractParsers {

  type Result[+T] <: MonadPlus[T, Result]
  trait AbstractParser[+L, +N, +T]
      extends ((
        Input[L @uncheckedVariance, N @uncheckedVariance],
        Int,
        SPPFLookup[L @uncheckedVariance, N @uncheckedVariance]) => Result[
        T @uncheckedVariance]) {
    def symbol: org.meerkat.tree.Symbol
    def reset(): Unit = {}
  }

//  implicit class ParserOps[-E, -N, +T] (parser:  AbstractParser[L, N, T]) extends AnyRef {
//    def withI
//  }

  type AbstractSequence[L, N, +T] = AbstractParser[L, N, T] with Slot {
    def size: Int; def symbol: org.meerkat.tree.Sequence
  }

  type AbstractAlternation[L, N, +T] = AbstractParser[L, N, T] {
    def symbol: org.meerkat.tree.Alt
  }

  type AbstractSymbol[L, N, +T, +V] = AbstractParser[L, N, T] {
    def name: String
    def action: Option[Any => V]
  }

  type AbstractNonterminal[L, N, +T, +V] = AbstractSymbol[L, N, T, V] {
    def symbol: org.meerkat.tree.NonterminalSymbol
  }

  type Head[L, N] = AbstractNonterminal[L, N, Any, Any]

  type AbstractSequenceBuilder[L, N, +T, +V] =
    (Slot => AbstractSequence[L, N, T]) { def action: Option[Any => V] }
  type AbstractAlternationBuilder[L, N, +T, +V] =
    (Head[L, N] => AbstractAlternation[L, N, T]) {
      def action: Option[Any => V]
    }

  trait CanBuildSequence[L, N, A, B, ValA, ValB] {
    implicit val m1: Memoizable[A]
    implicit val m2: Memoizable[B]

    type T; type V

    type Sequence <: AbstractSequence[L, N, T]
    def sequence(p: AbstractSequence[L, N, T]): Sequence

    def index(a: A): Int
    def intermediate(a: A, b: B, p: Slot, sppfLookup: SPPFLookup[L, N]): T

    type SequenceBuilder <: (Slot => Sequence) { def action: Option[Any => V] }
    def builderSeq(f: Slot => Sequence): SequenceBuilder
  }

  trait CanBuildAlternative[L, N, A] {
    implicit val m: Memoizable[A]
    def result(e: A, p: Slot, nt: Head[L, N], sppfLookup: SPPFLookup[L, N]): A
  }

  trait CanBuildAlternation[L, N, A, B, ValA, ValB] {
    implicit val m1: Memoizable[A]
    implicit val m2: Memoizable[B]

    implicit val o1: CanBuildAlternative[L, N, A]
    implicit val o2: CanBuildAlternative[L, N, B]

    type Alternation <: AbstractAlternation[L, N, B]
    def alternation(f: AbstractParser[L, N, B]): Alternation

    type AlternationBuilder <: (Head[L, N] => Alternation) {
      def action: Option[Any => ValB]
    }
    def builderAlt(f: Head[L, N] => Alternation): AlternationBuilder
  }

  trait CanBuildNonterminal[L, N, A, ValA] {
    implicit val m: Memoizable[A]
    type Nonterminal <: AbstractNonterminal[L, N, A, ValA]
    def nonterminal(name: String, p: AbstractParser[L, N, A]): Nonterminal

    type Symbol <: AbstractSymbol[L, N, A, ValA]
    def symbol(p: AbstractSymbol[L, N, A, ValA]): Symbol
  }

  trait CanBuildNegative[L, N, A, ValA] {
    implicit val m: Memoizable[A]
    type Symbol <: AbstractSymbol[L, N, A, ValA]
    type Nonterminal <: AbstractNonterminal[L, N, A, ValA]

    def not(name: String, p: AbstractParser[L, N, A]): Nonterminal
    def not(p: AbstractSymbol[L, N, A, ValA]): Symbol
  }

  trait CanBuildEBNF[L, N, A, ValA] {
    implicit val m: Memoizable[A]

    type Regular <: AbstractNonterminal[L, N, A, ValA]
    type Group <: AbstractNonterminal[L, N, A, ValA]

    def regular(symbol: org.meerkat.tree.NonterminalSymbol,
                p: AbstractParser[L, N, A]): Regular
    def group(p: AbstractParser[L, N, A]): Group
  }

  trait CanMap[L, N, A, B, Val] {
    implicit val m: Memoizable[A]
    type Nonterminal <: AbstractNonterminal[L, N, B, Val]
    def nonterminal(name: String, p: AbstractParser[L, N, B]): Nonterminal

    def index(a: A): Int
    def intermediate(a: A, b: B, p: Slot, sppfLookup: SPPFLookup[L, N]): B

    type Sequence <: AbstractSequence[L, N, B]
    def sequence(p: AbstractSequence[L, N, B]): Sequence

    type SequenceBuilder <: (Slot => Sequence) {
      def action: Option[Any => Val]
    }
    def builderSeq(f: Slot => Sequence): SequenceBuilder
  }

  object AbstractParser {

    def seq[L, N, A, B, ValA, ValB](p1: AbstractSequenceBuilder[L, N, A, ValA],
                                    p2: AbstractSymbol[L, N, B, ValB])(
        implicit builder: CanBuildSequence[L, N, A, B, ValA, ValB]
    ): builder.SequenceBuilder = builder.builderSeq { slot =>
      val q1 = p1(slot); sequence(slot, q1, p2, q1.size + 1)
    }

    def seq[L, N, A, B, ValA, ValB](p1: AbstractSymbol[L, N, A, ValA],
                                    p2: AbstractSymbol[L, N, B, ValB])(
        implicit builder: CanBuildSequence[L, N, A, B, ValA, ValB]
    ): builder.SequenceBuilder = builder.builderSeq { slot =>
      sequence(slot, p1, p2, 2)
    }

    /*protected def sequence[A,B,ValA,ValB](slot: Slot, p1: AbstractParser[L, N,A], p2: AbstractParser[L, N,B], s: Int)
                                         (implicit builder: CanBuildSequence[L, N,A,B,ValA,ValB]): builder.Sequence = { import builder._
      builder sequence (new AbstractParser[L, N,T] with Slot {
        def apply(input: Input[L, N], i: Int, sppfLookup: SPPFLookup[L, N]) = p1(input,i,sppfLookup) flatMap { x => p2(input,index(x),sppfLookup).smap { intermediate(x,_,this,sppfLookup) } }
        def size = s; def symbol = org.meerkat.tree.Sequence(p1.symbol, p2.symbol)
        def ruleType = org.meerkat.tree.PartialRule(slot.ruleType.head, slot.ruleType.body, s)
        override def reset = { p1.reset; p2.reset }
        override def toString = s"[${ruleType.toString()},$size]"
      })
    }*/
    protected def sequence[L, N, A, B, ValA, ValB](slot: Slot,
                                                   p1: AbstractParser[L, N, A],
                                                   p2: AbstractParser[L, N, B],
                                                   s: Int)(
        implicit builder: CanBuildSequence[L, N, A, B, ValA, ValB]
    ): builder.Sequence = {
      import builder._
      builder.sequence(new AbstractParser[L, N, T] with Slot {
        def apply(input: Input[L, N],
                  i: Int,
                  sppfLookup: SPPFLookup[L, N]): Result[builder.T] =
          p1(input, i, sppfLookup) flatMap { x =>
            p2(input, index(x), sppfLookup).smap {
              intermediate(x, _, this, sppfLookup)
            }
          }

        def size: Int = s

        def symbol = org.meerkat.tree.Sequence(p1.symbol, p2.symbol)

        def ruleType =
          org.meerkat.tree
            .PartialRule(slot.ruleType.head, slot.ruleType.body, s)

        override def reset(): Unit = {
          p1.reset()
          p2.reset()
        }

        override def toString = s"[${ruleType.toString()},$size]"
      })
    }

    def altAlt[L, N, A, B >: A, ValA, ValB >: ValA](
        p1: AbstractAlternationBuilder[L, N, A, ValA],
        p2: AbstractAlternationBuilder[L, N, B, ValB]
    )(implicit builder: CanBuildAlternation[L, N, A, B, ValA, ValB])
      : builder.AlternationBuilder = builder.builderAlt { head =>
      alternation(p1(head), p2(head))
    }

    def altAltSeq[L, N, A, B >: A, ValA, ValB >: ValA](
        p1: AbstractAlternationBuilder[L, N, A, ValA],
        p2: AbstractSequenceBuilder[L, N, B, ValB]
    )(implicit builder: CanBuildAlternation[L, N, A, B, ValA, ValB])
      : builder.AlternationBuilder = {
      import builder._
      builderAlt { head =>
        this.alternation(p1(head), alt(head, p2))
      }
    }

    def altSeqAlt[L, N, A, B >: A, ValA, ValB >: ValA](
        p1: AbstractSequenceBuilder[L, N, A, ValA],
        p2: AbstractAlternationBuilder[L, N, B, ValB]
    )(implicit builder: CanBuildAlternation[L, N, A, B, ValA, ValB])
      : builder.AlternationBuilder = {
      import builder._
      builderAlt { head =>
        this.alternation(alt(head, p1), p2(head))
      }
    }

    def altAltSym[L, N, A, B >: A, ValA, ValB >: ValA](
        p1: AbstractAlternationBuilder[L, N, A, ValA],
        p2: AbstractSymbol[L, N, B, ValB])(
        implicit builder: CanBuildAlternation[L, N, A, B, ValA, ValB]
    ): builder.AlternationBuilder = {
      import builder._
      builderAlt { head =>
        this.alternation(p1(head), alt(head, p2))
      }
    }

    def altSymAlt[L, N, A, B >: A, ValA, ValB >: ValA](
        p1: AbstractSymbol[L, N, A, ValA],
        p2: AbstractAlternationBuilder[L, N, B, ValB])(
        implicit builder: CanBuildAlternation[L, N, A, B, ValA, ValB]
    ): builder.AlternationBuilder = {
      import builder._
      builderAlt { head =>
        this.alternation(alt(head, p1), p2(head))
      }
    }

    def altSeq[L, N, A, B >: A, ValA, ValB >: ValA](
        p1: AbstractSequenceBuilder[L, N, A, ValA],
        p2: AbstractSequenceBuilder[L, N, B, ValB]
    )(implicit builder: CanBuildAlternation[L, N, A, B, ValA, ValB])
      : builder.AlternationBuilder = {
      import builder._
      builderAlt { head =>
        this.alternation(alt(head, p1), alt(head, p2))
      }
    }

    def altSymSeq[L, N, A, B >: A, ValA, ValB >: ValA](
        p1: AbstractSymbol[L, N, A, ValA],
        p2: AbstractSequenceBuilder[L, N, B, ValB])(
        implicit builder: CanBuildAlternation[L, N, A, B, ValA, ValB]
    ): builder.AlternationBuilder = {
      import builder._
      builderAlt { head =>
        this.alternation(alt(head, p1), alt(head, p2))
      }
    }

    def altSeqSym[L, N, A, B >: A, ValA, ValB >: ValA](
        p1: AbstractSequenceBuilder[L, N, A, ValA],
        p2: AbstractSymbol[L, N, B, ValB])(
        implicit builder: CanBuildAlternation[L, N, A, B, ValA, ValB]
    ): builder.AlternationBuilder = {
      import builder._
      builder.builderAlt { head =>
        this.alternation(alt(head, p1), alt(head, p2))
      }
    }

    def altSym[L, N, A, B >: A, ValA, ValB >: ValA](
        p1: AbstractSymbol[L, N, A, ValA],
        p2: AbstractSymbol[L, N, B, ValB])(
        implicit builder: CanBuildAlternation[L, N, A, B, ValA, ValB]
    ): builder.AlternationBuilder = {
      import builder._
      builderAlt { head =>
        this.alternation(alt(head, p1), alt(head, p2))
      }
    }

    def alt[L, N, B, Val](head: Head[L, N],
                          p: AbstractSequenceBuilder[L, N, B, Val])(
        implicit builder: CanBuildAlternative[L, N, B]) = {
      import builder._
      new AbstractParser[L, N, B] with Slot {
        val q = p(this)
        def apply(input: Input[L, N], i: Int, sppfLookup: SPPFLookup[L, N]) =
          q(input, i, sppfLookup).map { x =>
            builder.result(x, this, head, sppfLookup)
          }
        def symbol = q.symbol
        lazy val ruletype = {
          val rule = org.meerkat.tree.Rule(head.symbol, this.symbol);
          rule.action = p.action; rule
        }
        def ruleType               = ruletype
        override def reset(): Unit = q.reset()
        override def toString      = s"p${this.hashCode}"
      }
    }

    def alt[L, N, B, Val](head: Head[L, N], p: AbstractSymbol[L, N, B, Val])(
        implicit builder: CanBuildAlternative[L, N, B]) = {
      import builder._
      new AbstractParser[L, N, B] with Slot {
        def apply(input: Input[L, N], i: Int, sppfLookup: SPPFLookup[L, N]) =
          p(input, i, sppfLookup).map { x =>
            builder.result(x, this, head, sppfLookup)
          }
        def symbol = p.symbol
        lazy val ruletype = {
          val rule = org.meerkat.tree.Rule(head.symbol, this.symbol);
          rule.action = p.action; rule
        }
        def ruleType          = ruletype
        override def reset    = p.reset
        override def toString = s"p${this.hashCode}"
      }
    }

    protected def alternation[L, N, A, B >: A, ValA, ValB](
        p1: AbstractParser[L, N, A],
        p2: AbstractParser[L, N, B])(
        implicit builder: CanBuildAlternation[L, N, A, B, ValA, ValB]
    ): builder.Alternation =
      builder.alternation(new AbstractParser[L, N, B] {
        def apply(input: Input[L, N], i: Int, sppfLookup: SPPFLookup[L, N]) =
          p1(input, i, sppfLookup).orElse(p2(input, i, sppfLookup))
        def symbol = org.meerkat.tree.Alt(p1.symbol, p2.symbol)
        override def reset(): Unit = {
          p1.reset()
          p2.reset()
        }
      })

    def map[L, N, A, B, ValA](p: AbstractSequenceBuilder[L, N, A, ValA],
                              f: A => B)(
        implicit builder: CanMap[L, N, A, B, ValA]): builder.SequenceBuilder = {
      import builder._
      builderSeq { slot =>
        val q = p(slot)
        builder.sequence(new AbstractParser[L, N, B] with Slot {
          def apply(input: Input[L, N], i: Int, sppfLookup: SPPFLookup[L, N]) =
            q(input, i, sppfLookup) map f
          def size                   = q.size
          def ruleType               = q.ruleType
          def symbol                 = q.symbol
          override def reset(): Unit = q.reset()
        })
      }
    }

    def map[L, N, A, B, ValA](p: AbstractSymbol[L, N, A, ValA], f: A => B)(
        implicit builder: CanMap[L, N, A, B, ValA]): builder.Nonterminal = {
      import builder._
      nonterminal(
        p.name,
        new AbstractParser[L, N, B] {
          def apply(input: Input[L, N], i: Int, sppfLookup: SPPFLookup[L, N]) =
            p(input, i, sppfLookup) map f
          def symbol                 = p.symbol
          override def reset(): Unit = p.reset()
        }
      )
    }

    def map[L, N, A, B, ValA](p: AbstractSymbol[L, N, A, ValA],
                              f: (Input[L, N], A) => B)(
        implicit builder: CanMap[L, N, A, B, ValA]): builder.Nonterminal = {
      import builder._
      nonterminal(
        p.name,
        new AbstractParser[L, N, B] {
          def apply(input: Input[L, N], i: Int, sppfLookup: SPPFLookup[L, N]) =
            p(input, i, sppfLookup) map { f(input, _) }
          def symbol                 = p.symbol
          override def reset(): Unit = p.reset()
        }
      )
    }

    def flatMap[L, N, A, B, ValA, ValB](
        p: AbstractSequenceBuilder[L, N, A, ValA],
        f: A => AbstractSymbol[L, N, B, ValB])(
        implicit builder: CanBuildSequence[L, N, A, B, ValA, ValB]
    ): builder.SequenceBuilder = {
      import builder._
      builderSeq { slot =>
        val q = p(slot)
        builder.sequence(new AbstractParser[L, N, builder.T] with Slot {
          def apply(input: Input[L, N], i: Int, sppfLookup: SPPFLookup[L, N]) =
            q(input, i, sppfLookup) flatMap { x =>
              f(x)(input, index(x), sppfLookup).smap {
                intermediate(x, _, this, sppfLookup)
              }
            }
          def size = q.size + 1
          def symbol =
            org.meerkat.tree.Sequence(
              q.symbol,
              org.meerkat.tree.SimpleNonterminal(s"${f.hashCode}"))
          def ruleType =
            org.meerkat.tree.PartialRule(slot.ruleType.head,
                                         slot.ruleType.body,
                                         size)
          override def toString      = s"[${ruleType.toString()},$size]"
          override def reset(): Unit = q.reset()
        })
      }
    }

//    def flatMap2[A,B,ValA,ValB](p: AbstractSymbol[L, N,A,ValA], f: A => AbstractSymbol[L, N,B,ValB])(implicit builder: CanBuildSequence[L, N,A,B,ValA,ValB]): builder.SequenceBuilder = {
//      import builder._
//      builderSeq { slot =>
//        builder.sequence(new AbstractParser[L, N,builder.T] with Slot {
//          def apply(input: Input[L, N], i: Int, sppfLookup: SPPFLookup[L, N]) = p(input,i,sppfLookup) flatMap { x => f(x)(input,index(x),sppfLookup).smap { intermediate(x,_,this,sppfLookup) } }
//          def size = 2; def symbol = org.meerkat.tree.Sequence(p.symbol, org.meerkat.tree.SimpleNonterminal(s"${f.hashCode}"))
//          def ruleType = org.meerkat.tree.PartialRule(slot.ruleType.head, slot.ruleType.body, size)
//          override def toString = s"[${ruleType.toString()},$size]"
//          override def reset = p.reset
//        }) }
//    }
  }
}

object AbstractCPSParsers extends AbstractParsers {
  import AbstractParser._

  type Result[+T] = CPSResult[T]

  def negativeSym[L, N, A, ValA](
      name: String,
      p: => AbstractSymbol[L, N, A, ValA]
  )(implicit builder: CanBuildNegative[L, N, A, ValA],
    obj: ClassTag[Result[A]]): builder.Nonterminal = {
    import builder._
    lazy val q: Nonterminal = not(name, p); q
  }

  def nonterminalSym[L, N, A, ValA](name: String,
                                    p: => AbstractSymbol[L, N, A, ValA])(
      implicit builder: CanBuildNonterminal[L, N, A, ValA],
      b: CanBuildAlternative[L, N, A],
      obj: ClassTag[Result[A]]
  ): builder.Nonterminal = {
    import builder._
    lazy val q: Nonterminal = nonterminal(name, memoize(alt(q, p))); q
  }

  def nonterminalSeq[L, N, A, ValA](
      name: String,
      p: => AbstractSequenceBuilder[L, N, A, ValA])(
      implicit builder: CanBuildNonterminal[L, N, A, ValA],
      b: CanBuildAlternative[L, N, A],
      obj: ClassTag[Result[A]]
  ): builder.Nonterminal = {
    import builder._
    lazy val q: Nonterminal = nonterminal(name, memoize(alt(q, p))); q
  }

  def nonterminalAlt[L, N, A, ValA](
      name: String,
      p: => AbstractAlternationBuilder[L, N, A, ValA]
  )(implicit builder: CanBuildNonterminal[L, N, A, ValA],
    obj: ClassTag[Result[A]]): builder.Nonterminal = {
    import builder._
    lazy val q: Nonterminal = builder.nonterminal(name, memoize(p(q))); q
  }

  def regular[L, N, A, ValA](
      symbol: org.meerkat.tree.NonterminalSymbol,
      p: => AbstractAlternationBuilder[L, N, A, ValA]
  )(implicit builder: CanBuildEBNF[L, N, A, ValA],
    obj: ClassTag[Result[A]]): builder.Regular = {
    import builder._
    lazy val q: Regular = builder.regular(symbol, memoize(p(q))); q
  }

  def groupSeq[L, N, A, ValA](
      p: => AbstractSequenceBuilder[L, N, A, _]
  )(implicit builder: CanBuildEBNF[L, N, A, ValA],
    b: CanBuildAlternative[L, N, A],
    obj: ClassTag[Result[A]]): builder.Group = {
    import builder._
    lazy val q: Group = builder.group(memoize(alt(q, p))); q
  }

  def groupAlt[L, N, A, ValA](p: => AbstractAlternationBuilder[L, N, A, _])(
      implicit builder: CanBuildEBNF[L, N, A, ValA],
      obj: ClassTag[Result[A]]): builder.Group = {
    import builder._
    lazy val q: Group = builder.group(memoize(p(q))); q
  }

  def preFilter[L, N, B, Val](p: AbstractSymbol[L, N, B, Val],
                              pred: (Input[L, N], Int) => Boolean,
                              prefix: String)(
      implicit builder: CanBuildNonterminal[L, N, B, Val]
  ): builder.Symbol =
    builder.symbol(new AbstractParser[L, N, B] {
      def apply(input: Input[L, N], i: Int, sppfLookup: SPPFLookup[L, N]) =
        if (pred(input, i)) p(input, i, sppfLookup) else CPSResult.failure[B]
      def name              = s"$prefix ${p.name}"
      override def toString = name
      def symbol = p.symbol match {
        case nt: NonterminalSymbol => NonterminalSymbol(name)
        case EdgeSymbol(_)         => EdgeSymbol(name)
        case _                     => throw new RuntimeException("Shouldn't have happened!")
      }
      def action: Option[Any => Val] = None
      override def reset(): Unit     = p.reset()
    })

  def postFilter[L, N, B, Val](p: AbstractSymbol[L, N, B, Val],
                               pred: (Input[L, N], B) => Boolean,
                               postfix: String)(
      implicit builder: CanBuildNonterminal[L, N, B, Val]
  ): builder.Symbol =
    builder.symbol(new AbstractParser[L, N, B] {
      def apply(input: Input[L, N], i: Int, sppfLookup: SPPFLookup[L, N]) =
        p(input, i, sppfLookup).filter { pred(input, _) }
      def name                      = s"${p.name} $postfix"
      override def toString: String = name
      def symbol = p.symbol match {
        case nt: NonterminalSymbol => NonterminalSymbol(name)
        case EdgeSymbol(_)         => EdgeSymbol(name)
        case _                     => throw new RuntimeException("Shouldn't have happened!")
      }
      def action: Option[Any => Val] = None
      override def reset(): Unit     = p.reset()
    })

  import CPSResult.memo

  protected def memoize[L, N, A: Memoizable](
      p: => AbstractParser[L, N, A]
  )(implicit obj: ClassTag[Result[A]]): AbstractParser[L, N, A] = {
    lazy val q: AbstractParser[L, N, A] = p
    val results                         = new mutable.HashMap[Int, Result[A]]()
    new AbstractParser[L, N, A] {
      def apply(input: Input[L, N], i: Int, sppfLookup: SPPFLookup[L, N]) =
        results.get(i) match {
          case Some(res) => res
          case _ =>
            val res = memo(q(input, i, sppfLookup))
            results += (i -> res)
            res
        }
      def symbol = q.symbol
      override def reset(): Unit = {
        val done = results.isEmpty
        if (!done) {
          results.clear()
          q.reset()
        }
      }
    }
  }

}
