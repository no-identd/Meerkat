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

import org.meerkat.sppf.{NonPackedNode, SPPFLookup, Slot, TerminalNode}
import org.meerkat.util.Input
import org.meerkat.tree

import scala.util.matching.Regex
import scala.collection.mutable
import org.meerkat.tree.{TerminalSymbol, VertexSymbol}

object Parsers {
  import AbstractCPSParsers._

  implicit def obj1[ValA, ValB](implicit vals: ValA |~| ValB) =
    new CanBuildSequence[NonPackedNode, NonPackedNode, ValA, ValB] {
      implicit val m1 = obj4; implicit val m2 = obj4

      type T = NonPackedNode; type V = vals.R

      type Sequence = Parsers.Sequence
      def sequence(p: AbstractSequence[NonPackedNode]): Sequence = new Sequence {
        def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p(input, i, sppfLookup)
        def size                                                      = p.size; def symbol = p.symbol; def ruleType = p.ruleType
        override def reset                                            = p.reset
      }
      def index(a: T): Int                                             = a.rightExtent
      def intermediate(a: T, b: T, p: Slot, sppfLookup: SPPFLookup): T = sppfLookup.getIntermediateNode(p, a, b)

      type SequenceBuilder = Parsers.SequenceBuilder[V]
      def builderSeq(f: Slot => Sequence) = new Parsers.SequenceBuilder[V] { def apply(slot: Slot) = f(slot) }
    }

  implicit object obj2 extends CanBuildAlternative[NonPackedNode] {
    implicit val m                                                          = obj4
    def result(e: NonPackedNode, p: Slot, nt: Head, sppfLookup: SPPFLookup) = sppfLookup.getNonterminalNode(nt, p, e)
  }

  implicit def obj3[ValA, ValB] = new CanBuildAlternation[NonPackedNode, NonPackedNode, ValA, ValB] {

    implicit val m1 = obj4; implicit val m2 = obj4
    implicit val o1 = obj2; implicit val o2 = obj2

    type Alternation = Parsers.Alternation
    def alternation(p: AbstractParser[NonPackedNode]): Alternation = new Alternation {
      def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p(input, i, sppfLookup)
      def symbol                                                    = p.symbol.asInstanceOf[org.meerkat.tree.Alt]
      override def reset                                            = p.reset
    }
    type AlternationBuilder = Parsers.AlternationBuilder[ValB]
    def builderAlt(f: Head => Alternation) = new Parsers.AlternationBuilder[ValB] { def apply(head: Head) = f(head) }
  }
  //Возвращает правый предел
  implicit object obj4 extends Memoizable[NonPackedNode] {
    type U = Int
    def value(t: NonPackedNode): Int = t.rightExtent;
  }

  implicit def obj5[Val] = new CanBuildNonterminal[NonPackedNode, Val] {
    implicit val m = obj4

    type Nonterminal = Parsers.AbstractNonterminal[Val]
    def nonterminal(nt: String, p: AbstractParser[NonPackedNode]) = new Parsers.AbstractNonterminal[Val] {
      def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p(input, i, sppfLookup)
      def symbol                                                    = org.meerkat.tree.SimpleNonterminal(nt)
      def name                                                      = nt; override def toString = name
      override def reset                                            = p.reset
    }

    type Symbol = Parsers.Symbol[Val]
    def symbol(p: AbstractSymbol[NonPackedNode, Val]) = new Parsers.Symbol[Val] {
      def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p(input, i, sppfLookup)
      def name                                                      = p.name; def symbol = p.symbol
      override def reset                                            = p.reset
    }
  }

  implicit def obj6[Val] = new CanBuildEBNF[NonPackedNode, Val] {
    implicit val m = obj4

    type T       = NonPackedNode
    type Regular = AbstractNonterminal[Val]
    type Group   = AbstractNonterminal[Val]

    def regular(sym: org.meerkat.tree.NonterminalSymbol, p: AbstractParser[NonPackedNode]): Regular =
      new AbstractNonterminal[Val] {
        def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p(input, i, sppfLookup)
        def name                                                      = symbol.toString; def symbol = sym
        override def toString                                         = name
        override def reset                                            = p.reset
      }
    def group(p: AbstractParser[NonPackedNode]): Group = new AbstractNonterminal[Val] {
      def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = p(input, i, sppfLookup)
      def name                                                      = symbol.toString; def symbol = org.meerkat.tree.Group(p.symbol)
      override def toString                                         = name
      override def reset                                            = p.reset
    }
  }


  implicit def obj8[Val] = new CanBuildNegative[NonPackedNode, Val] {
    implicit val m = obj4

    type Nonterminal = Parsers.AbstractNonterminal[Val]
    def not(nt: String, p: AbstractParser[NonPackedNode]) = new Parsers.AbstractNonterminal[Val] {
      def apply(input: Input, i: Int, sppfLookup: SPPFLookup) =
        p(input, if (i == 0) Int.MinValue else -i, sppfLookup)
      def symbol         = org.meerkat.tree.SimpleNonterminal(nt)
      def name           = "-" + nt; override def toString = name
      override def reset = p.reset
    }

    type Symbol = Parsers.Symbol[Val]
    def not(p: AbstractSymbol[NonPackedNode, Val]) = new Parsers.Symbol[Val] {
      def apply(input: Input, i: Int, sppfLookup: SPPFLookup) =
        p(input, if (i == 0) Int.MinValue else -i, sppfLookup)
      def name           = "-" + p.name; def symbol = p.symbol
      override def reset = p.reset
    }
  }

  trait Sequence extends AbstractParser[NonPackedNode] with Slot {
    def size: Int; def symbol: org.meerkat.tree.Sequence
  }
  trait Alternation extends AbstractParser[NonPackedNode] { def symbol: org.meerkat.tree.Alt }

  trait AbstractNonterminal[+V] extends Symbol[V] {
    def symbol: org.meerkat.tree.NonterminalSymbol; type Abstract[+X] = AbstractNonterminal[X]
  }
  type Nonterminal = AbstractNonterminal[NoValue]

  trait Terminal extends Symbol[NoValue] { def symbol: org.meerkat.tree.TerminalSymbol }
  trait Vertex   extends Symbol[NoValue] { def symbol: org.meerkat.tree.VertexSymbol   }

  val ε = new Terminal {
    def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = CPSResult.success(sppfLookup.getEpsilonNode(i))
    def symbol                                                    = TerminalSymbol(name)
    def name                                                      = "epsilon"; override def toString = name
  }

  // TODO: disallow terminals/nonterminals to be defined as epsilon
  val epsilon = ε

  trait SequenceBuilder[+V] extends (Slot => Sequence) with SequenceBuilderOps[V] {
    import AbstractParser._
    def action: Option[Any => V] = None

    def ~[U](p: Symbol[U])(implicit tuple: V |~| U) = this ~~ p
    def ~~[U](p: Symbol[U])(implicit tuple: V |~| U)                = seq(this, p)


    def &[U](f: V => U) = new SequenceBuilderWithAction[U] {
      def apply(slot: Slot) = SequenceBuilder.this(slot)
      def action            =
        /*Option ({ x => x match
                            {case g: Set[V] => g.map(i => f(i))
                              case _=> f(x.asInstanceOf[V])}})*/ Option({ x =>
          f(x.asInstanceOf[V])
        })
    }

    def ^[U](f: String => U)(implicit sub: V <:< NoValue) = new SequenceBuilderWithAction[U] {
      def apply(slot: Slot) = SequenceBuilder.this(slot)
      def action =
        Option({ x =>
          f(x.asInstanceOf[String])
        })
    }

    var group: Option[AbstractNonterminal[_]] = None
    def !(implicit ebnf: EBNF[V]): AbstractNonterminal[ebnf.Group] = {
      type T = AbstractNonterminal[ebnf.Group]
      group
        .asInstanceOf[Option[T]]
        .getOrElse({
          val p = groupSeq(this); group = Option(p); p
        })
    }

    def ~[U](q: OperatorParsers.AbstractOperatorNonterminal[U])(implicit tuple: V |~| U) =
      this ~~ q
    def ~~[U](q: OperatorParsers.AbstractOperatorNonterminal[U])(implicit tuple: V |~| U) =
      AbstractOperatorParsers.AbstractOperatorParser.seqSeqNt(this, q)(
        OperatorParsers.OperatorImplicits.obj1[V, U](tuple)
      )
  }

  trait SequenceBuilderWithAction[+V] extends (Slot => Sequence) with SequenceBuilderOps[V] {
    import AbstractParser._
    def action: Option[Any => V]
  }

  trait SequenceBuilderOps[+V] extends (Slot => Sequence) {
    import AbstractParser._
    def action: Option[Any => V]

    def |[U >: V](p: AlternationBuilder[U]) = altSeqAlt(this, p)
    def |[U >: V](p: SequenceBuilder[U])    = altSeq(this, p)
    def |[U >: V](p: Symbol[U])             = altSeqSym(this, p)

    def |[U >: V](q: SequenceBuilderWithAction[U]) = altSeq(this, q)
    def |[U >: V](q: SymbolWithAction[U])          = altSeqSym(this, q)
  }

  trait AlternationBuilder[+V] extends (Head => Alternation) {
    import AbstractParser._
    def action: Option[Any => V] = None

    def |[U >: V](p: AlternationBuilder[U]) = altAlt(this, p)
    def |[U >: V](p: SequenceBuilder[U])    = altAltSeq(this, p)
    def |[U >: V](p: Symbol[U])             = altAltSym(this, p)

    def |[U >: V](q: SequenceBuilderWithAction[U]) = altAltSeq(this, q)
    def |[U >: V](q: SymbolWithAction[U])          = altAltSym(this, q)

    var group: Option[AbstractNonterminal[_]] = None
    def !(implicit ebnf: EBNF[V]): AbstractNonterminal[ebnf.Group] = {
      type T = AbstractNonterminal[ebnf.Group]
      group.asInstanceOf[Option[T]].getOrElse({ val p = groupAlt(this); group = Option(p); p })
    }
  }

  trait Symbol[+V] extends AbstractParser[NonPackedNode] with SymbolOps[V] with EBNFs[V] {
    import AbstractParser._
    def name: String
    def action: Option[Any => V] = None

    def ~[U](p: Symbol[U])(implicit tuple: V |~| U) = this ~~ p
    def ~~[U](p: Symbol[U])(implicit tuple: V |~| U)                = seq(this, p)

    def ~(p: String)        = this ~~ p
    def ~~(p: String)(implicit tuple: V |~| NoValue) = seq(this, p)

    def &[U](f: V => U) = new SymbolWithAction[U] {
      def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = Symbol.this(input, i, sppfLookup)
      def name                                                      = Symbol.this.name; def symbol = Symbol.this.symbol
      def action =
        Option({ x =>
          f(x.asInstanceOf[V])
        })
      override def reset = Symbol.this.reset
    }
    def ^[U](f: String => U)(implicit sub: V <:< NoValue) = new SymbolWithAction[U] {
      def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = Symbol.this(input, i, sppfLookup)
      def name                                                      = Symbol.this.name; def symbol = Symbol.this.symbol
      def action =
        Option({ x =>
          f(x.asInstanceOf[String])
        })
      override def reset = Symbol.this.reset
    }
  }

  trait SymbolWithAction[+V] extends AbstractParser[NonPackedNode] with SymbolOps[V] {
    import AbstractParser._
    def name: String
    def action: Option[Any => V]
  }

  trait SymbolOps[+V] extends AbstractParser[NonPackedNode] {
    import AbstractParser._
    def name: String
    def action: Option[Any => V]

    def |[U >: V](p: AlternationBuilder[U]) = altSymAlt(this, p)
    def |[U >: V](p: SequenceBuilder[U])    = altSymSeq(this, p)
    def |[U >: V](p: Symbol[U])             = altSym(this, p)

    def |[U >: V](q: SequenceBuilderWithAction[U]) = altSymSeq(this, q)
    def |[U >: V](q: SymbolWithAction[U])          = altSym(this, q)
  }

  implicit def toTerminal(label: String): Terminal =
    terminal(label)


  def terminal(label: String): Terminal = new Terminal {
    def apply(input: Input, i: Int, sppfLookup: SPPFLookup) =
      input.filterEdges(i, label) match {
        case edges if edges.isEmpty => CPSResult.failure
        case edges =>
          val terminals = edges.map { to =>
            CPSResult.success(sppfLookup.getTerminalNode(label, i, to))
          }
          terminals.reduceLeft(_.orElse(_))
      }
    override def name: String     = label
    override def symbol           = TerminalSymbol(label)
    override def toString: String = name
  }


  def ntAlt[Val](name: String, p: => AlternationBuilder[Val])            = nonterminalAlt(name, p)
  def ntSeq[Val](name: String, p: => SequenceBuilder[Val])               = nonterminalSeq(name, p)
  def ntSym[Val](name: String, p: => AbstractSymbol[NonPackedNode, Val]) = nonterminalSym(name, p)

  def notSym[Val](name: String, p: => AbstractSymbol[NonPackedNode, Val]) = negativeSym(name, p)

  trait EBNFs[+V] { self: Symbol[V] =>
    var opt: Option[AbstractNonterminal[_]] = None
    def ?(implicit ebnf: EBNF[V]): AbstractNonterminal[ebnf.OptOrSeq] = {
      type T = AbstractNonterminal[ebnf.OptOrSeq]
      opt
        .asInstanceOf[Option[T]]
        .getOrElse({
          val p =
            regular[NonPackedNode, ebnf.OptOrSeq](org.meerkat.tree.Opt(this.symbol), this & ebnf.unit | ε ^ ebnf.empty)
          opt = Option(p); p
        })
    }

    var star: Option[AbstractNonterminal[_]] = None
    def *(implicit ebnf: EBNF[V]): AbstractNonterminal[ebnf.OptOrSeq] = {
      type T = AbstractNonterminal[ebnf.OptOrSeq]
      star
        .asInstanceOf[Option[T]]
        .getOrElse({
          val p = regular[NonPackedNode, ebnf.OptOrSeq](
            org.meerkat.tree.Star(this.symbol),
            star.asInstanceOf[Option[T]].get ~ this & ebnf.add | ε ^ ebnf.empty
          )
          star = Option(p); p
        })
    }

    var starstar: Option[AbstractNonterminal[_]] = None
    def **(implicit ebnf: EBNF[V]): AbstractNonterminal[ebnf.OptOrSeq] = {
      type T = AbstractNonterminal[ebnf.OptOrSeq]
      starstar
        .asInstanceOf[Option[T]]
        .getOrElse({
          val p = regular[NonPackedNode, ebnf.OptOrSeq](
            org.meerkat.tree.Star(this.symbol),
            starstar.asInstanceOf[Option[T]].get ~~ this & ebnf.add | ε ^ ebnf.empty
          )
          starstar = Option(p); p
        })
    }

    val star_sep: mutable.Map[String, AbstractNonterminal[_]] = mutable.HashMap.empty
    def *(sep: Terminal)(implicit ebnf: EBNF[V]): AbstractNonterminal[ebnf.OptOrSeq] = {
      type T = AbstractNonterminal[ebnf.OptOrSeq]
      star_sep
        .getOrElseUpdate(sep.name, {
          regular[NonPackedNode, ebnf.OptOrSeq](org.meerkat.tree.Star(this.symbol), this.+(sep) | ε ^ ebnf.empty)
        })
        .asInstanceOf[T]
    }

    var plus: Option[AbstractNonterminal[_]] = None
    def +(implicit ebnf: EBNF[V]): AbstractNonterminal[ebnf.OptOrSeq] = {
      type T = AbstractNonterminal[ebnf.OptOrSeq]
      plus
        .asInstanceOf[Option[T]]
        .getOrElse({
          val p = regular[NonPackedNode, ebnf.OptOrSeq](
            org.meerkat.tree.Plus(this.symbol),
            plus.asInstanceOf[Option[T]].get ~ this & ebnf.add | this & ebnf.unit
          )
          plus = Option(p); p
        })
    }

    var plusplus: Option[AbstractNonterminal[_]] = None
    def ++(implicit ebnf: EBNF[V]): AbstractNonterminal[ebnf.OptOrSeq] = {
      type T = AbstractNonterminal[ebnf.OptOrSeq]
      plusplus
        .asInstanceOf[Option[T]]
        .getOrElse({
          val p = regular[NonPackedNode, ebnf.OptOrSeq](
            org.meerkat.tree.Plus(this.symbol),
            plusplus.asInstanceOf[Option[T]].get ~~ this & ebnf.add | this & ebnf.unit
          )
          plusplus = Option(p); p
        })
    }

    val plus_sep: mutable.Map[String, AbstractNonterminal[_]] = mutable.HashMap.empty
    def +(sep: Terminal)(implicit ebnf: EBNF[V]): AbstractNonterminal[ebnf.OptOrSeq] = {
      type T = AbstractNonterminal[ebnf.OptOrSeq]
      plus_sep
        .getOrElseUpdate(sep.name, {
          regular[NonPackedNode, ebnf.OptOrSeq](
            org.meerkat.tree.Plus(this.symbol),
            plus_sep(sep.name).asInstanceOf[T] ~ sep ~ this & ebnf.add | this & ebnf.unit
          )
        })
        .asInstanceOf[T]

    }
  }

}
