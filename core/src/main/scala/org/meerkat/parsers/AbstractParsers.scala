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

import org.meerkat.sppf.SPPFLookup
import org.meerkat.util.Input

import scala.reflect.ClassTag
import org.meerkat.sppf.Slot
import org.meerkat.tree.NonterminalSymbol
import org.meerkat.tree.TerminalSymbol

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
//  type Func[E, N, +T] = (Input [E, N], Int, SPPFLookup[E]) => Result[T]
  trait AbstractParser[E, N, +T] extends  ((Input [E, N], Int, SPPFLookup[E]) => Result[T]) {
    def symbol: org.meerkat.tree.Symbol
    def reset(): Unit = {}
  }

  type AbstractSequence[E, N,+T] = AbstractParser[E, N,T] with Slot { def size: Int; def symbol: org.meerkat.tree.Sequence }

  type AbstractAlternation[E, N,+T] = AbstractParser[E, N,T] { def symbol: org.meerkat.tree.Alt }

  type AbstractSymbol[E, N, +T, +V] = AbstractParser[E, N,T] { def name: String; def action: Option[Any => V] }

  type AbstractNonterminal[E, N,+T, +V] = AbstractSymbol[E, N,T, V] { def symbol: org.meerkat.tree.NonterminalSymbol }

  type Head[E, N] = AbstractNonterminal[E, N,Any, Any]

  type AbstractSequenceBuilder[E, N,+T, +V]    = (Slot => AbstractSequence[E, N,T]) { def action: Option[Any => V] }
  type AbstractAlternationBuilder[E, N,+T, +V] = (Head[E, N] => AbstractAlternation[E, N,T]) { def action: Option[Any => V] }

  trait CanBuildSequence[E, N,A, B, ValA, ValB] {
    implicit val m1: Memoizable[A]
    implicit val m2: Memoizable[B]

    type T; type V

    type Sequence <: AbstractSequence[E, N,T]
    def sequence(p: AbstractSequence[E, N,T]): Sequence

    def index(a: A): Int
    def intermediate(a: A, b: B, p: Slot, sppfLookup: SPPFLookup[E]): T

    type SequenceBuilder <: (Slot => Sequence) { def action: Option[Any => V] }
    def builderSeq(f: Slot => Sequence): SequenceBuilder
  }

  trait CanBuildAlternative[E, N,A] {
    implicit val m: Memoizable[A]
    def result(e: A, p: Slot, nt: Head[E, N], sppfLookup: SPPFLookup[E]): A
  }

  trait CanBuildAlternation[E, N,A, B, ValA, ValB] {
    implicit val m1: Memoizable[A]
    implicit val m2: Memoizable[B]

    implicit val o1: CanBuildAlternative[E, N,A]
    implicit val o2: CanBuildAlternative[E, N,B]

    type Alternation <: AbstractAlternation[E, N,B]
    def alternation(f: AbstractParser[E, N,B]): Alternation

    type AlternationBuilder <: (Head[E, N] => Alternation) { def action: Option[Any => ValB] }
    def builderAlt(f: Head[E, N] => Alternation): AlternationBuilder
  }

  trait CanBuildNonterminal[E, N,A, ValA] {
    implicit val m: Memoizable[A]
    type Nonterminal <: AbstractNonterminal[E, N,A, ValA]
    def nonterminal(name: String, p: AbstractParser[E, N,A]): Nonterminal

    type Symbol <: AbstractSymbol[E, N,A, ValA]
    def symbol(p: AbstractSymbol[E, N,A, ValA]): Symbol
  }

  trait CanBuildNegative[E, N,A, ValA] {
    implicit val m: Memoizable[A]
    type Symbol <: AbstractSymbol[E, N,A, ValA]
    type Nonterminal <: AbstractNonterminal[E, N,A, ValA]

    def not(name: String, p: AbstractParser[E, N,A]): Nonterminal
    def not(p: AbstractSymbol[E, N,A, ValA]): Symbol
  }

  trait CanBuildEBNF[E, N, A, ValA] {
    implicit val m: Memoizable[A]

    type Regular <: AbstractNonterminal[E, N,A, ValA]
    type Group <: AbstractNonterminal[E, N,A, ValA]

    def regular(symbol: org.meerkat.tree.NonterminalSymbol, p: AbstractParser[E, N,A]): Regular
    def group(p: AbstractParser[E, N,A]): Group
  }

  trait CanMap[E, N,A, B, Val] {
    implicit val m: Memoizable[A]
    type Nonterminal <: AbstractNonterminal[E, N,B, Val]
    def nonterminal(name: String, p: AbstractParser[E, N,B]): Nonterminal

    def index(a: A): Int
    def intermediate(a: A, b: B, p: Slot, sppfLookup: SPPFLookup[E]): B

    type Sequence <: AbstractSequence[E, N,B]
    def sequence(p: AbstractSequence[E, N,B]): Sequence

    type SequenceBuilder <: (Slot => Sequence) { def action: Option[Any => Val] }
    def builderSeq(f: Slot => Sequence): SequenceBuilder
  }

  object AbstractParser {

    def seq[E, N, A, B, ValA, ValB](p1: AbstractSequenceBuilder[E, N,A, ValA], p2: AbstractSymbol[E, N,B, ValB])(
      implicit builder: CanBuildSequence[E, N,A, B, ValA, ValB]
    ): builder.SequenceBuilder = builder.builderSeq { slot =>
      val q1 = p1(slot); sequence(slot, q1, p2, q1.size + 1)
    }

    def seq[E, N, A, B, ValA, ValB](p1: AbstractSymbol[E, N,A, ValA], p2: AbstractSymbol[E, N,B, ValB])(
      implicit builder: CanBuildSequence[E, N,A, B, ValA, ValB]
    ): builder.SequenceBuilder = builder.builderSeq { slot =>
      sequence(slot, p1, p2, 2)
    }

    /*protected def sequence[A,B,ValA,ValB](slot: Slot, p1: AbstractParser[E, N,A], p2: AbstractParser[E, N,B], s: Int)
                                         (implicit builder: CanBuildSequence[E, N,A,B,ValA,ValB]): builder.Sequence = { import builder._
      builder sequence (new AbstractParser[E, N,T] with Slot {
        def apply(input: Input[E, N], i: Int, sppfLookup: SPPFLookup[E]) = p1(input,i,sppfLookup) flatMap { x => p2(input,index(x),sppfLookup).smap { intermediate(x,_,this,sppfLookup) } }
        def size = s; def symbol = org.meerkat.tree.Sequence(p1.symbol, p2.symbol)
        def ruleType = org.meerkat.tree.PartialRule(slot.ruleType.head, slot.ruleType.body, s)
        override def reset = { p1.reset; p2.reset }
        override def toString = s"[${ruleType.toString()},$size]"
      })
    }*/
    protected def sequence[E, N, A, B, ValA, ValB](slot: Slot, p1: AbstractParser[E, N,A], p2: AbstractParser[E, N,B], s: Int)(
      implicit builder: CanBuildSequence[E, N,A, B, ValA, ValB]
    ): builder.Sequence = {
      import builder._
      builder.sequence(new AbstractParser[E, N,T] with Slot {
        def apply(input: Input[E, N], i: Int, sppfLookup: SPPFLookup[E]): Result[builder.T] = p1(input, i, sppfLookup) flatMap {
          x =>
            p2(input, index(x), sppfLookup).smap {
              intermediate(x, _, this, sppfLookup)
            }
        }

        def size: Int = s

        def symbol = org.meerkat.tree.Sequence(p1.symbol, p2.symbol)

        def ruleType = org.meerkat.tree.PartialRule(slot.ruleType.head, slot.ruleType.body, s)

        override def reset(): Unit = {
          p1.reset()
          p2.reset()
        }

        override def toString = s"[${ruleType.toString()},$size]"
      })
    }

    def altAlt[E, N, A, B >: A, ValA, ValB >: ValA](
      p1: AbstractAlternationBuilder[E, N,A, ValA],
      p2: AbstractAlternationBuilder[E, N,B, ValB]
    )(implicit builder: CanBuildAlternation[E, N,A, B, ValA, ValB]): builder.AlternationBuilder = builder.builderAlt {
      head =>
        alternation(p1(head), p2(head))
    }

    def altAltSeq[E, N, A, B >: A, ValA, ValB >: ValA](
      p1: AbstractAlternationBuilder[E, N,A, ValA],
      p2: AbstractSequenceBuilder[E, N,B, ValB]
    )(implicit builder: CanBuildAlternation[E, N,A, B, ValA, ValB]): builder.AlternationBuilder = {
      import builder._
      builderAlt { head =>
        this.alternation(p1(head), alt(head, p2))
      }
    }

    def altSeqAlt[E, N, A, B >: A, ValA, ValB >: ValA](
      p1: AbstractSequenceBuilder[E, N,A, ValA],
      p2: AbstractAlternationBuilder[E, N,B, ValB]
    )(implicit builder: CanBuildAlternation[E, N,A, B, ValA, ValB]): builder.AlternationBuilder = {
      import builder._
      builderAlt { head =>
        this.alternation(alt(head, p1), p2(head))
      }
    }

    def altAltSym[E, N, A, B >: A, ValA, ValB >: ValA](p1: AbstractAlternationBuilder[E, N,A, ValA], p2: AbstractSymbol[E, N,B, ValB])(
      implicit builder: CanBuildAlternation[E, N,A, B, ValA, ValB]
    ): builder.AlternationBuilder = {
      import builder._
      builderAlt { head =>
        this.alternation(p1(head), alt(head, p2))
      }
    }

    def altSymAlt[E, N, A, B >: A, ValA, ValB >: ValA](p1: AbstractSymbol[E, N,A, ValA], p2: AbstractAlternationBuilder[E, N,B, ValB])(
      implicit builder: CanBuildAlternation[E, N,A, B, ValA, ValB]
    ): builder.AlternationBuilder = {
      import builder._
      builderAlt { head =>
        this.alternation(alt(head, p1), p2(head))
      }
    }

    def altSeq[E, N, A, B >: A, ValA, ValB >: ValA](
      p1: AbstractSequenceBuilder[E, N,A, ValA],
      p2: AbstractSequenceBuilder[E, N,B, ValB]
    )(implicit builder: CanBuildAlternation[E, N,A, B, ValA, ValB]): builder.AlternationBuilder = {
      import builder._
      builderAlt { head =>
        this.alternation(alt(head, p1), alt(head, p2))
      }
    }

    def altSymSeq[E, N, A, B >: A, ValA, ValB >: ValA](p1: AbstractSymbol[E, N,A, ValA], p2: AbstractSequenceBuilder[E, N,B, ValB])(
      implicit builder: CanBuildAlternation[E, N,A, B, ValA, ValB]
    ): builder.AlternationBuilder = {
      import builder._
      builderAlt { head =>
        this.alternation(alt(head, p1), alt(head, p2))
      }
    }

    def altSeqSym[E, N, A, B >: A, ValA, ValB >: ValA](p1: AbstractSequenceBuilder[E, N,A, ValA], p2: AbstractSymbol[E, N,B, ValB])(
      implicit builder: CanBuildAlternation[E, N,A, B, ValA, ValB]
    ): builder.AlternationBuilder = {
      import builder._
      builder.builderAlt { head =>
        this.alternation(alt(head, p1), alt(head, p2))
      }
    }

    def altSym[E, N, A, B >: A, ValA, ValB >: ValA](p1: AbstractSymbol[E, N,A, ValA], p2: AbstractSymbol[E, N,B, ValB])(
      implicit builder: CanBuildAlternation[E, N,A, B, ValA, ValB]
    ): builder.AlternationBuilder = {
      import builder._
      builderAlt { head =>
        this.alternation(alt(head, p1), alt(head, p2))
      }
    }

    def alt[E, N, B, Val](head: Head[E, N], p: AbstractSequenceBuilder[E, N,B, Val])(implicit builder: CanBuildAlternative[E, N,B]) = {
      import builder._
      new AbstractParser[E, N,B] with Slot {
        val q = p(this)
        def apply(input: Input[E, N], i: Int, sppfLookup: SPPFLookup[E]) = q(input, i, sppfLookup).map { x =>
          builder.result(x, this, head, sppfLookup)
        }
        def symbol                 = q.symbol
        lazy val ruletype          = { val rule = org.meerkat.tree.Rule(head.symbol, this.symbol); rule.action = p.action; rule }
        def ruleType               = ruletype
        override def reset(): Unit = q.reset()
        override def toString      = s"p${this.hashCode}"
      }
    }

    def alt[E, N, B, Val](head: Head[E, N], p: AbstractSymbol[E, N,B, Val])(implicit builder: CanBuildAlternative[E, N,B]) = {
      import builder._
      new AbstractParser[E, N,B] with Slot {
        def apply(input: Input[E, N], i: Int, sppfLookup: SPPFLookup[E]) = p(input, i, sppfLookup).map { x =>
          builder.result(x, this, head, sppfLookup)
        }
        def symbol            = p.symbol
        lazy val ruletype     = { val rule = org.meerkat.tree.Rule(head.symbol, this.symbol); rule.action = p.action; rule }
        def ruleType          = ruletype
        override def reset    = p.reset
        override def toString = s"p${this.hashCode}"
      }
    }

    protected def alternation[E, N, A, B >: A, ValA, ValB](p1: AbstractParser[E, N,A], p2: AbstractParser[E, N,B])(
      implicit builder: CanBuildAlternation[E, N,A, B, ValA, ValB]
    ): builder.Alternation =
      builder.alternation(new AbstractParser[E, N,B] {
        def apply(input: Input[E, N], i: Int, sppfLookup: SPPFLookup[E]) =
          p1(input, i, sppfLookup).orElse(p2(input, i, sppfLookup))
        def symbol = org.meerkat.tree.Alt(p1.symbol, p2.symbol)
        override def reset(): Unit = {
          p1.reset()
          p2.reset()
        }
      })

    def map[E, N, A, B, ValA](p: AbstractSequenceBuilder[E, N,A, ValA],
                        f: A => B)(implicit builder: CanMap[E, N,A, B, ValA]): builder.SequenceBuilder = {
      import builder._
      builderSeq { slot =>
        val q = p(slot)
        builder.sequence(new AbstractParser[E, N,B] with Slot {
          def apply(input: Input[E, N], i: Int, sppfLookup: SPPFLookup[E]) = q(input, i, sppfLookup) map f
          def size                                                = q.size
          def ruleType                                            = q.ruleType
          def symbol                                              = q.symbol
          override def reset(): Unit                              = q.reset()
        })
      }
    }

    def map[E, N, A, B, ValA](p: AbstractSymbol[E, N,A, ValA],
                        f: A => B)(implicit builder: CanMap[E, N,A, B, ValA]): builder.Nonterminal = {
      import builder._
      nonterminal(p.name, new AbstractParser[E, N,B] {
        def apply(input: Input[E, N], i: Int, sppfLookup: SPPFLookup[E]) = p(input, i, sppfLookup) map f
        def symbol                                              = p.symbol
        override def reset(): Unit                              = p.reset()
      })
    }

    def map[E, N, A, B, ValA](p: AbstractSymbol[E, N,A, ValA],
                        f: (Input[E, N], A) => B)(implicit builder: CanMap[E, N,A, B, ValA]): builder.Nonterminal = {
      import builder._
      nonterminal(p.name, new AbstractParser[E, N,B] {
        def apply(input: Input[E, N], i: Int, sppfLookup: SPPFLookup[E]) = p(input, i, sppfLookup) map { f(input, _) }
        def symbol                                              = p.symbol
        override def reset(): Unit                              = p.reset()
      })
    }

    def flatMap[E, N, A, B, ValA, ValB](p: AbstractSequenceBuilder[E, N,A, ValA], f: A => AbstractSymbol[E, N,B, ValB])(
      implicit builder: CanBuildSequence[E, N,A, B, ValA, ValB]
    ): builder.SequenceBuilder = {
      import builder._
      builderSeq { slot =>
        val q = p(slot)
        builder.sequence(new AbstractParser[E, N,builder.T] with Slot {
          def apply(input: Input[E, N], i: Int, sppfLookup: SPPFLookup[E]) =
            q(input, i, sppfLookup) flatMap { x =>
              f(x)(input, index(x), sppfLookup).smap {
                intermediate(x, _, this, sppfLookup)
              }
            }
          def size                   = q.size + 1
          def symbol                 = org.meerkat.tree.Sequence(q.symbol, org.meerkat.tree.SimpleNonterminal(s"${f.hashCode}"))
          def ruleType               = org.meerkat.tree.PartialRule(slot.ruleType.head, slot.ruleType.body, size)
          override def toString      = s"[${ruleType.toString()},$size]"
          override def reset(): Unit = q.reset()
        })
      }
    }

//    def flatMap2[A,B,ValA,ValB](p: AbstractSymbol[E, N,A,ValA], f: A => AbstractSymbol[E, N,B,ValB])(implicit builder: CanBuildSequence[E, N,A,B,ValA,ValB]): builder.SequenceBuilder = {
//      import builder._
//      builderSeq { slot =>
//        builder.sequence(new AbstractParser[E, N,builder.T] with Slot {
//          def apply(input: Input[E, N], i: Int, sppfLookup: SPPFLookup[E]) = p(input,i,sppfLookup) flatMap { x => f(x)(input,index(x),sppfLookup).smap { intermediate(x,_,this,sppfLookup) } }
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

  def negativeSym[E, N, A, ValA](
    name: String,
    p: => AbstractSymbol[E, N,A, ValA]
  )(implicit builder: CanBuildNegative[E, N,A, ValA], obj: ClassTag[Result[A]]): builder.Nonterminal = {
    import builder._
    lazy val q: Nonterminal = not(name, p); q
  }

  def nonterminalSym[E, N, A, ValA](name: String, p: => AbstractSymbol[E, N,A, ValA])(
    implicit builder: CanBuildNonterminal[E, N,A, ValA],
    b: CanBuildAlternative[E, N,A],
    obj: ClassTag[Result[A]]
  ): builder.Nonterminal = {
    import builder._
    lazy val q: Nonterminal = nonterminal(name, memoize(alt(q, p))); q
  }

  def nonterminalSeq[E, N, A, ValA](name: String, p: => AbstractSequenceBuilder[E, N,A, ValA])(
    implicit builder: CanBuildNonterminal[E, N,A, ValA],
    b: CanBuildAlternative[E, N,A],
    obj: ClassTag[Result[A]]
  ): builder.Nonterminal = {
    import builder._
    lazy val q: Nonterminal = nonterminal(name, memoize(alt(q, p))); q
  }

  def nonterminalAlt[E, N, A, ValA](
    name: String,
    p: => AbstractAlternationBuilder[E, N,A, ValA]
  )(implicit builder: CanBuildNonterminal[E, N,A, ValA], obj: ClassTag[Result[A]]): builder.Nonterminal = {
    import builder._
    lazy val q: Nonterminal = builder.nonterminal(name, memoize(p(q))); q
  }


  def regular[E, N, A, ValA](
    symbol: org.meerkat.tree.NonterminalSymbol,
    p: => AbstractAlternationBuilder[E, N,A, ValA]
  )(implicit builder: CanBuildEBNF[E, N, A, ValA], obj: ClassTag[Result[A]]): builder.Regular = {
    import builder._
    lazy val q: Regular = builder.regular(symbol, memoize(p(q))); q
  }

  def groupSeq[E, N, A, ValA](
    p: => AbstractSequenceBuilder[E, N,A, _]
  )(implicit builder: CanBuildEBNF[E, N, A, ValA], b: CanBuildAlternative[E, N,A], obj: ClassTag[Result[A]]): builder.Group = {
    import builder._
    lazy val q: Group = builder.group(memoize(alt(q, p))); q
  }

  def groupAlt[E, N, A, ValA](p: => AbstractAlternationBuilder[E, N,A, _])(implicit builder: CanBuildEBNF[E, N, A, ValA],
                                                                obj: ClassTag[Result[A]]): builder.Group = {
    import builder._
    lazy val q: Group = builder.group(memoize(p(q))); q
  }

  def preFilter[E, N, B, Val](p: AbstractSymbol[E, N,B, Val], pred: (Input[E, N], Int) => Boolean, prefix: String)(
    implicit builder: CanBuildNonterminal[E, N,B, Val]
  ): builder.Symbol =
    builder.symbol(new AbstractParser[E, N,B] {
      def apply(input: Input[E, N], i: Int, sppfLookup: SPPFLookup[E]) =
        if (pred(input, i)) p(input, i, sppfLookup) else CPSResult.failure[B]
      def name              = s"$prefix ${p.name}"
      override def toString = name
      def symbol = p.symbol match {
        case nt: NonterminalSymbol => NonterminalSymbol(name)
        case TerminalSymbol(_)     => TerminalSymbol(name)
        case _                     => throw new RuntimeException("Shouldn't have happened!")
      }
      def action: Option[Any => Val] = None
      override def reset(): Unit     = p.reset()
    })

  def postFilter[E, N, B, Val](p: AbstractSymbol[E, N,B, Val], pred: (Input[E, N], B) => Boolean, postfix: String)(
    implicit builder: CanBuildNonterminal[E, N,B, Val]
  ): builder.Symbol =
    builder.symbol(new AbstractParser[E, N,B] {
      def apply(input: Input[E, N], i: Int, sppfLookup: SPPFLookup[E]) = p(input, i, sppfLookup).filter { pred(input, _) }
      def name                                                = s"${p.name} $postfix"
      override def toString: String                           = name
      def symbol = p.symbol match {
        case nt: NonterminalSymbol => NonterminalSymbol(name)
        case TerminalSymbol(_)     => TerminalSymbol(name)
        case _                     => throw new RuntimeException("Shouldn't have happened!")
      }
      def action: Option[Any => Val] = None
      override def reset(): Unit     = p.reset()
    })

  import CPSResult.memo

  protected def memoize[E, N, A: Memoizable](
    p: => AbstractParser[E, N,A]
  )(implicit obj: ClassTag[Result[A]]): AbstractParser[E, N,A] = {
    lazy val q: AbstractParser[E, N,A] = p
    val results                   = new mutable.HashMap[Int, Result[A]]()
    new AbstractParser[E, N,A] {
      def apply(input: Input[E, N], i: Int, sppfLookup: SPPFLookup[E]) =
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
