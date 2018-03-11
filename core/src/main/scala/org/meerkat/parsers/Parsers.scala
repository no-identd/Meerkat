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

  implicit def obj1[E, N, ValA, ValB](implicit vals: ValA |~| ValB) =
    new CanBuildSequence[E, N,NonPackedNode, NonPackedNode, ValA, ValB] {
      implicit val m1 = obj4; implicit val m2 = obj4

      type T = NonPackedNode; type V = vals.R

      type Sequence = Parsers.Sequence[E, N]
      def sequence(p: AbstractSequence[E, N,NonPackedNode]): Sequence = new Sequence {
        def apply(input: Input[E, N], i: Int, sppfLookup: SPPFLookup[E]) = p(input, i, sppfLookup)
        def size                                                      = p.size; def symbol = p.symbol; def ruleType = p.ruleType
        override def reset                                            = p.reset
      }
      def index(a: T): Int                                             = a.rightExtent
      def intermediate(a: T, b: T, p: Slot, sppfLookup: SPPFLookup[E]): T = sppfLookup.getIntermediateNode(p, a, b)

      type SequenceBuilder = Parsers.SequenceBuilder[E, N,V]
      def builderSeq(f: Slot => Sequence) = new Parsers.SequenceBuilder[E, N,V] { def apply(slot: Slot) = f(slot) }
    }

  implicit def obj2[E, N] = new  CanBuildAlternative[E, N,NonPackedNode] {
    implicit val m                                                          = obj4
    def result(e: NonPackedNode, p: Slot, nt: Head[E, N], sppfLookup: SPPFLookup[E]) = sppfLookup.getNonterminalNode(nt, p, e)
  }

  implicit def obj3[E, N, ValA, ValB] = new CanBuildAlternation[E, N,NonPackedNode, NonPackedNode, ValA, ValB] {

    implicit val m1 = obj4; implicit val m2 = obj4
    implicit val o1 = obj2; implicit val o2 = obj2

    type Alternation = Parsers.Alternation[E, N]
    def alternation(p: AbstractParser[E, N,NonPackedNode]): Alternation = new Alternation {
      def apply(input: Input[E, N], i: Int, sppfLookup: SPPFLookup[E]) = p(input, i, sppfLookup)
      def symbol                                                    = p.symbol.asInstanceOf[org.meerkat.tree.Alt]
      override def reset                                            = p.reset
    }
    type AlternationBuilder = Parsers.AlternationBuilder[E, N,ValB]
    def builderAlt(f: Head[E, N] => Alternation) = new Parsers.AlternationBuilder[E, N,ValB] { def apply(head: Head[E, N]) = f(head) }
  }
  //Возвращает правый предел
  implicit object obj4 extends Memoizable[NonPackedNode] {
    type U = Int
    def value(t: NonPackedNode): Int = t.rightExtent;
  }

  implicit def obj5[E, N, Val] = new CanBuildNonterminal[E, N,NonPackedNode, Val] {
    implicit val m = obj4

    type Nonterminal = Parsers.AbstractNonterminal[E, N,Val]
    def nonterminal(nt: String, p: AbstractParser[E, N,NonPackedNode]) = new Parsers.AbstractNonterminal[E, N,Val] {
      def apply(input: Input[E, N], i: Int, sppfLookup: SPPFLookup[E]) = p(input, i, sppfLookup)
      def symbol                                                    = org.meerkat.tree.SimpleNonterminal(nt)
      def name                                                      = nt; override def toString = name
      override def reset                                            = p.reset
    }

    type Symbol = Parsers.Symbol[E, N, Val]
    def symbol(p: AbstractSymbol[E, N,NonPackedNode, Val]) = new Parsers.Symbol[E, N, Val] {
      def apply(input: Input[E, N], i: Int, sppfLookup: SPPFLookup[E]) = p(input, i, sppfLookup)
      def name                                                      = p.name; def symbol = p.symbol
      override def reset                                            = p.reset
    }
  }

  implicit def obj6[E, N, Val] = new CanBuildEBNF[E, N, NonPackedNode, Val] {
    implicit val m = obj4

    type T       = NonPackedNode
    type Regular = AbstractNonterminal[E, N,Val]
    type Group   = AbstractNonterminal[E, N,Val]

    def regular(sym: org.meerkat.tree.NonterminalSymbol, p: AbstractParser[E, N,NonPackedNode]): Regular =
      new AbstractNonterminal[E, N,Val] {
        def apply(input: Input[E, N], i: Int, sppfLookup: SPPFLookup[E]) = p(input, i, sppfLookup)
        def name                                                      = symbol.toString; def symbol = sym
        override def toString                                         = name
        override def reset                                            = p.reset
      }
    def group(p: AbstractParser[E, N,NonPackedNode]): Group = new AbstractNonterminal[E, N,Val] {
      def apply(input: Input[E, N], i: Int, sppfLookup: SPPFLookup[E]) = p(input, i, sppfLookup)
      def name                                                      = symbol.toString; def symbol = org.meerkat.tree.Group(p.symbol)
      override def toString                                         = name
      override def reset                                            = p.reset
    }
  }


  implicit def obj8[E, N, Val] = new CanBuildNegative[E, N,NonPackedNode, Val] {
    implicit val m = obj4

    type Nonterminal = Parsers.AbstractNonterminal[E, N,Val]
    def not(nt: String, p: AbstractParser[E, N,NonPackedNode]) = new Parsers.AbstractNonterminal[E, N,Val] {
      def apply(input: Input[E, N], i: Int, sppfLookup: SPPFLookup[E]) =
        p(input, if (i == 0) Int.MinValue else -i, sppfLookup)
      def symbol         = org.meerkat.tree.SimpleNonterminal(nt)
      def name           = "-" + nt; override def toString = name
      override def reset = p.reset
    }

    type Symbol = Parsers.Symbol[E, N, Val]
    def not(p: AbstractSymbol[E, N,NonPackedNode, Val]) = new Parsers.Symbol[E, N, Val] {
      def apply(input: Input[E, N], i: Int, sppfLookup: SPPFLookup[E]) =
        p(input, if (i == 0) Int.MinValue else -i, sppfLookup)
      def name           = "-" + p.name; def symbol = p.symbol
      override def reset = p.reset
    }
  }

  trait Sequence[E, N] extends AbstractParser[E, N,NonPackedNode] with Slot {
    def size: Int; def symbol: org.meerkat.tree.Sequence
  }
  trait Alternation[E, N]  extends AbstractParser[E, N, NonPackedNode] { def symbol: org.meerkat.tree.Alt }

  trait AbstractNonterminal[E, N,+V] extends Symbol[E, N, V] {
    def symbol: org.meerkat.tree.NonterminalSymbol
    type Abstract[+X] = AbstractNonterminal[E, N, X]
  }
  type Nonterminal[E, N] = AbstractNonterminal[E, N,NoValue]

  trait Terminal[+E] extends Symbol[E, Nothing, NoValue] { def symbol: org.meerkat.tree.TerminalSymbol }
  trait Vertex[+N]   extends Symbol[Nothing, N, NoValue] { def symbol: org.meerkat.tree.VertexSymbol   }

  def ε = new Terminal[Nothing] {
    def apply(input: Input[Nothing, Nothing], i: Int, sppfLookup: SPPFLookup[Nothing]) =
      CPSResult.success(sppfLookup.getEpsilonNode(i))
    def symbol                                                    = TerminalSymbol(name)
    def name                                                      = "epsilon"
    override def toString = name

  }

  // TODO: disallow terminals/nonterminals to be defined as epsilon
  def epsilon = ε

  trait SequenceBuilder[E, N, +V] extends (Slot => Sequence[E, N]) with SequenceBuilderOps[E, N,V] {
    import AbstractParser._
    def action: Option[Any => V] = None

    def ~[U](p: Symbol[E, N, U])(implicit tuple: V |~| U)                = seq(this, p)

    def &[U](f: V => U) = new SequenceBuilderWithAction[E, N,U] {
      def apply(slot: Slot) = SequenceBuilder.this(slot)
      def action            =
        /*Option ({ x => x match
                            {case g: Set[V] => g.map(i => f(i))
                              case _=> f(x.asInstanceOf[V])}})*/ Option({ x =>
          f(x.asInstanceOf[V])
        })
    }

    def ^[U](f: String => U)(implicit sub: V <:< NoValue) = new SequenceBuilderWithAction[E, N,U] {
      def apply(slot: Slot) = SequenceBuilder.this(slot)
      def action =
        Option({ x =>
          f(x.asInstanceOf[String])
        })
    }

    var group: Option[AbstractNonterminal[E, N,_]] = None
    def !(implicit ebnf: EBNF[V]): AbstractNonterminal[E, N,ebnf.Group] = {
      type T = AbstractNonterminal[E, N,ebnf.Group]
      group
        .asInstanceOf[Option[T]]
        .getOrElse({
          val p = groupSeq(this); group = Option(p); p
        })
    }

    def ~[U](q: OperatorParsers.AbstractOperatorNonterminal[E, N,U])(implicit tuple: V |~| U) =
      AbstractOperatorParsers.AbstractOperatorParser.seqSeqNt(this, q)(
        OperatorParsers.OperatorImplicits.obj1[E, N, V, U](tuple)
      )
  }

  trait SequenceBuilderWithAction[E, N, +V] extends (Slot => Sequence[E, N])
    with SequenceBuilderOps[E, N, V] {
    import AbstractParser._
    def action: Option[Any => V]
  }

  trait SequenceBuilderOps[E, N, +V] extends (Slot => Sequence[E, N]) {
    import AbstractParser._
    def action: Option[Any => V]

    def |[U >: V](p: AlternationBuilder[E, N,U]) = altSeqAlt(this, p)
    def |[U >: V](p: SequenceBuilder[E, N, U])    = altSeq(this, p)
    def |[U >: V](p: Symbol[E, N, U])             = altSeqSym(this, p)

    def |[U >: V](q: SequenceBuilderWithAction[E, N,U]) = altSeq(this, q)
    def |[U >: V](q: SymbolWithAction[E, N,U])          = altSeqSym(this, q)
  }

  trait AlternationBuilder[E, N,+V] extends (Head[E, N] => Alternation[E, N]) {
    import AbstractParser._
    def action: Option[Any => V] = None

    def |[U >: V](p: AlternationBuilder[E, N,U]) = altAlt(this, p)
    def |[U >: V](p: SequenceBuilder[E, N,U])    = altAltSeq(this, p)
    def |[U >: V](p: Symbol[E, N, U])             = altAltSym(this, p)

    def |[U >: V](q: SequenceBuilderWithAction[E, N,U]) = altAltSeq(this, q)
    def |[U >: V](q: SymbolWithAction[E, N,U])          = altAltSym(this, q)

    var group: Option[AbstractNonterminal[E, N,_]] = None
    def !(implicit ebnf: EBNF[V]): AbstractNonterminal[E, N,ebnf.Group] = {
      type T = AbstractNonterminal[E, N,ebnf.Group]
      group.asInstanceOf[Option[T]].getOrElse({ val p = groupAlt(this); group = Option(p); p })
    }
  }

  trait Symbol[+E, +N, +V] extends AbstractParser[E, N,NonPackedNode]
    with SymbolOps[E, N,V] with EBNFs[E, N,V] {
    import AbstractParser._
    def name: String
    def action: Option[Any => V] = None

    def ~[U, F >: E, G >: N](p: Symbol[F, G, U])(implicit tuple: V |~| U)                = seq(this, p)
//    def ~(p: String)(implicit tuple: V |~| NoValue) = seq(this, p)

    def &[U](f: V => U) = new SymbolWithAction[E, N,U] {
      def apply(input: Input[E, N], i: Int, sppfLookup: SPPFLookup[E]) = Symbol.this(input, i, sppfLookup)
      def name                                                      = Symbol.this.name; def symbol = Symbol.this.symbol
      def action =
        Option({ x =>
          f(x.asInstanceOf[V])
        })
      override def reset = Symbol.this.reset
    }
    def ^[U](f: String => U)(implicit sub: V <:< NoValue) = new SymbolWithAction[E, N,U] {
      def apply(input: Input[E, N], i: Int, sppfLookup: SPPFLookup[E]) = Symbol.this(input, i, sppfLookup)
      def name                                                      = Symbol.this.name; def symbol = Symbol.this.symbol
      def action =
        Option({ x =>
          f(x.asInstanceOf[String])
        })
      override def reset = Symbol.this.reset
    }
  }

  trait SymbolWithAction[+E, +N, +V] extends AbstractParser[E, N, NonPackedNode]
    with SymbolOps[E, N,V] {
    import AbstractParser._
    def name: String
    def action: Option[Any => V]
  }

  trait SymbolOps[+E, +N,+V] extends AbstractParser[E, N,NonPackedNode] {
    import AbstractParser._
    def name: String
    def action: Option[Any => V]

    def |[U >: V, F >: E, G >: N](p: AlternationBuilder[F, G, U]) = altSymAlt(this, p)
    def |[U >: V, F >: E, G >: N](p: SequenceBuilder[F, G, U])    = altSymSeq(this, p)
    def |[U >: V, F >: E, G >: N](p: Symbol[F, G, U])             = altSym(this, p)

    def |[U >: V, F >: E, G >: N](q: SequenceBuilderWithAction[F, G, U]) = altSymSeq(this, q)
    def |[U >: V, F >: E, G >: N](q: SymbolWithAction[F, G, U])          = altSym(this, q)
  }

  implicit def toTerminal[E](label: E): Terminal[E] =
    terminal(label)


  def terminal[E](label: E): Terminal[E] = new Terminal[E] {
    def apply(input: Input[E, Nothing], i: Int, sppfLookup: SPPFLookup[E]) =
      input.filterEdges(i, label) match {
        case edges if edges.isEmpty => CPSResult.failure
        case edges =>
          val terminals = edges.map { to =>
            CPSResult.success(sppfLookup.getTerminalNode(label, i, to))
          }
          terminals.reduceLeft(_.orElse(_))
      }
    
    override def name: String     = label.toString // TODO: add type signature 
    override def symbol           = TerminalSymbol(label)
    override def toString: String = name.toString
  }


  def ntAlt[E, N, Val](name: String, p: => AlternationBuilder[E, N,Val])            = nonterminalAlt(name, p)
  def ntSeq[E, N, Val](name: String, p: => SequenceBuilder[E, N,Val])               = nonterminalSeq(name, p)
  def ntSym[E, N,Val](name: String, p: => AbstractSymbol[E, N,NonPackedNode, Val]) = nonterminalSym(name, p)

  def notSym[E, N, Val](name: String, p: => AbstractSymbol[E, N,NonPackedNode, Val]) = negativeSym(name, p)

  // TODO: fix
  trait EBNFs[+E, +N, +V] {// self: Symbol[E, N, V] =>
//    var opt: Option[AbstractNonterminal[E, N,_]] = None
//    def ?(implicit ebnf: EBNF[V]): AbstractNonterminal[E, N,ebnf.OptOrSeq] = {
//      type T = AbstractNonterminal[E, N,ebnf.OptOrSeq]
//      opt
//        .asInstanceOf[Option[T]]
//        .getOrElse({
//          val p =
//            regular[E, N, NonPackedNode, ebnf.OptOrSeq](org.meerkat.tree.Opt(this.symbol),
//              this & ebnf.unit | ε ^ ebnf.empty)
//          opt = Option(p); p
//        })
//    }
//
//    var star: Option[AbstractNonterminal[E, N,_]] = None
//    def *(implicit ebnf: EBNF[V]): AbstractNonterminal[E, N,ebnf.OptOrSeq] = {
//      type T = AbstractNonterminal[E, N,ebnf.OptOrSeq]
//      star
//        .asInstanceOf[Option[T]]
//        .getOrElse({
//          val p = regular[E, N, NonPackedNode, ebnf.OptOrSeq](
//            org.meerkat.tree.Star(this.symbol),
//            star.asInstanceOf[Option[T]].get ~ this & ebnf.add | ε ^ ebnf.empty
//          )
//          star = Option(p); p
//        })
//    }



//    val star_sep: mutable.Map[String, AbstractNonterminal[E, N,_]] = mutable.HashMap.empty
//    def *(sep: Terminal[E])(implicit ebnf: EBNF[V]): AbstractNonterminal[E, N,ebnf.OptOrSeq] = {
//      type T = AbstractNonterminal[E, N,ebnf.OptOrSeq]
//      star_sep
//        .getOrElseUpdate(sep.name, {
//          regular[E, N, NonPackedNode, ebnf.OptOrSeq](org.meerkat.tree.Star(this.symbol), this.+(sep) | ε ^ ebnf.empty)
//        })
//        .asInstanceOf[T]
//    }

//    var plus: Option[AbstractNonterminal[E, N,_]] = None
//    def +(implicit ebnf: EBNF[V]): AbstractNonterminal[E, N,ebnf.OptOrSeq] = {
//      type T = AbstractNonterminal[E, N,ebnf.OptOrSeq]
//      plus
//        .asInstanceOf[Option[T]]
//        .getOrElse({
//          val p = regular[E, N, NonPackedNode, ebnf.OptOrSeq](
//            org.meerkat.tree.Plus(this.symbol),
//            plus.asInstanceOf[Option[T]].get ~ this & ebnf.add | this & ebnf.unit
//          )
//          plus = Option(p); p
//        })
//    }


//    val plus_sep: mutable.Map[String, AbstractNonterminal[E, N,_]] = mutable.HashMap.empty
//    def +(sep: E)(implicit ebnf: EBNF[V]): AbstractNonterminal[E, N,ebnf.OptOrSeq] = {
//      type T = AbstractNonterminal[E, N,ebnf.OptOrSeq]
//      plus_sep
//        .getOrElseUpdate(sep.name, {
//          regular[E, N, NonPackedNode, ebnf.OptOrSeq](
//            org.meerkat.tree.Plus(this.symbol),
//            plus_sep(sep.name).asInstanceOf[T] ~ terminal(sep) ~ this & ebnf.add | this & ebnf.unit
//          )
//        })
//        .asInstanceOf[T]
//
//    }
  }

}
