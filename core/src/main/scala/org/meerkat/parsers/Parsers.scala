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
import org.meerkat.input._
import org.meerkat.parsers.Parsers.SymbolWithAction
import org.meerkat.sppf.{NonPackedNode, SPPFLookup, Slot, TerminalNode}
import org.meerkat.tree

import scala.util.matching.Regex
import scala.collection.mutable
import org.meerkat.tree.{TerminalSymbol, VertexSymbol}

import scala.annotation.unchecked.uncheckedVariance
import scala.language.implicitConversions
import scala.util.Try

object Parsers {
  import AbstractCPSParsers._

  implicit def obj1[L, N, ValA, ValB](implicit vals: ValA |~| ValB) =
    new CanBuildSequence[L, N,NonPackedNode, NonPackedNode, ValA, ValB] {
      implicit val m1 = obj4; implicit val m2 = obj4

      type T = NonPackedNode; type V = vals.R

      type Sequence = Parsers.Sequence[L, N]
      def sequence(p: AbstractSequence[L, N,NonPackedNode]): Sequence = new Sequence {
        def apply(input: Input[L, N], i: Int, sppfLookup: SPPFLookup[L, N]) = p(input, i, sppfLookup)
        def size                                                      = p.size; def symbol = p.symbol; def ruleType = p.ruleType
        override def reset                                            = p.reset
      }
      def index(a: T): Int                                             = a.rightExtent
      def intermediate(a: T, b: T, p: Slot, sppfLookup: SPPFLookup[L, N]): T = sppfLookup.getIntermediateNode(p, a, b)

      type SequenceBuilder = Parsers.SequenceBuilder[L, N,V]
      def builderSeq(f: Slot => Sequence) = new Parsers.SequenceBuilder[L, N,V] { def apply(slot: Slot) = f(slot) }
    }

  implicit def obj2[L, N] = new  CanBuildAlternative[L, N,NonPackedNode] {
    implicit val m                                                          = obj4
    def result(e: NonPackedNode, p: Slot, nt: Head[L, N], sppfLookup: SPPFLookup[L, N]) = sppfLookup.getNonterminalNode(nt, p, e)
  }

  implicit def obj3[L, N, ValA, ValB] = new CanBuildAlternation[L, N,NonPackedNode, NonPackedNode, ValA, ValB] {

    implicit val m1 = obj4; implicit val m2 = obj4
    implicit val o1 = obj2; implicit val o2 = obj2

    type Alternation = Parsers.Alternation[L, N]
    def alternation(p: AbstractParser[L, N,NonPackedNode]): Alternation = new Alternation {
      def apply(input: Input[L, N], i: Int, sppfLookup: SPPFLookup[L, N]) = p(input, i, sppfLookup)
      def symbol                                                    = p.symbol.asInstanceOf[org.meerkat.tree.Alt]
      override def reset                                            = p.reset
    }
    type AlternationBuilder = Parsers.AlternationBuilder[L, N,ValB]
    def builderAlt(f: Head[L, N] => Alternation) = new Parsers.AlternationBuilder[L, N,ValB] { def apply(head: Head[L, N]) = f(head) }
  }
  //Возвращает правый предел
  implicit object obj4 extends Memoizable[NonPackedNode] {
    type U = Int
    def value(t: NonPackedNode): Int = t.rightExtent;
  }

  implicit def obj5[L, N, Val] = new CanBuildNonterminal[L, N,NonPackedNode, Val] {
    implicit val m = obj4

    type Nonterminal = Parsers.AbstractNonterminal[L, N,Val]
    def nonterminal(nt: String, p: AbstractParser[L, N,NonPackedNode]) = new Parsers.AbstractNonterminal[L, N,Val] {
      def apply(input: Input[L, N], i: Int, sppfLookup: SPPFLookup[L, N]) = p(input, i, sppfLookup)
      def symbol                                                    = org.meerkat.tree.SimpleNonterminal(nt)
      def name                                                      = nt; override def toString = name
      override def reset                                            = p.reset
    }

    type Symbol = Parsers.Symbol[L, N, Val]
    def symbol(p: AbstractSymbol[L, N,NonPackedNode, Val]) = new Parsers.Symbol[L, N, Val] {
      def apply(input: Input[L, N], i: Int, sppfLookup: SPPFLookup[L, N]) = p(input, i, sppfLookup)
      def name                                                      = p.name; def symbol = p.symbol
      override def reset                                            = p.reset
    }
  }

  implicit def obj6[L, N, Val] = new CanBuildEBNF[L, N, NonPackedNode, Val] {
    implicit val m = obj4

    type T       = NonPackedNode
    type Regular = AbstractNonterminal[L, N,Val]
    type Group   = AbstractNonterminal[L, N,Val]

    def regular(sym: org.meerkat.tree.NonterminalSymbol, p: AbstractParser[L, N,NonPackedNode]): Regular =
      new AbstractNonterminal[L, N,Val] {
        def apply(input: Input[L, N], i: Int, sppfLookup: SPPFLookup[L, N]) = p(input, i, sppfLookup)
        def name                                                      = symbol.toString; def symbol = sym
        override def toString                                         = name
        override def reset                                            = p.reset
      }
    def group(p: AbstractParser[L, N,NonPackedNode]): Group = new AbstractNonterminal[L, N,Val] {
      def apply(input: Input[L, N], i: Int, sppfLookup: SPPFLookup[L, N]) = p(input, i, sppfLookup)
      def name                                                      = symbol.toString; def symbol = org.meerkat.tree.Group(p.symbol)
      override def toString                                         = name
      override def reset                                            = p.reset
    }
  }


  implicit def obj8[L, N, Val] = new CanBuildNegative[L, N,NonPackedNode, Val] {
    implicit val m = obj4

    type Nonterminal = Parsers.AbstractNonterminal[L, N,Val]
    def not(nt: String, p: AbstractParser[L, N,NonPackedNode]) = new Parsers.AbstractNonterminal[L, N,Val] {
      def apply(input: Input[L, N], i: Int, sppfLookup: SPPFLookup[L, N]) =
        p(input, if (i == 0) Int.MinValue else -i, sppfLookup)
      def symbol         = org.meerkat.tree.SimpleNonterminal(nt)
      def name           = "-" + nt; override def toString = name
      override def reset = p.reset
    }

    type Symbol = Parsers.Symbol[L, N, Val]
    def not(p: AbstractSymbol[L, N,NonPackedNode, Val]) = new Parsers.Symbol[L, N, Val] {
      def apply(input: Input[L, N], i: Int, sppfLookup: SPPFLookup[L, N]) =
        p(input, if (i == 0) Int.MinValue else -i, sppfLookup)
      def name           = "-" + p.name; def symbol = p.symbol
      override def reset = p.reset
    }
  }

  trait Sequence[L, N] extends AbstractParser[L, N,NonPackedNode] with Slot {
    def size: Int; def symbol: org.meerkat.tree.Sequence
  }
  trait Alternation[L, N]  extends AbstractParser[L, N, NonPackedNode] { def symbol: org.meerkat.tree.Alt }

  trait AbstractNonterminal[+L, +N,+V] extends Symbol[L, N, V] {
    def symbol: org.meerkat.tree.NonterminalSymbol
    type Abstract[+X] = AbstractNonterminal[L @uncheckedVariance, N @uncheckedVariance, X]
  }

  type Nonterminal[+L , +N] = AbstractNonterminal[L @uncheckedVariance, N @uncheckedVariance,NoValue]

  trait Terminal[+L] extends Symbol[L, Nothing, NoValue] {
    def symbol: org.meerkat.tree.TerminalSymbol
    def ^^ = this.^(identity[L])
    def ^[U](f: L => U) = new SymbolWithAction[L, Nothing, U] {
      def apply(input: Input[L, Nothing], i: Int, sppfLookup: SPPFLookup[L, Nothing]) = Terminal.this(input, i, sppfLookup)
      def name = Terminal.this.name
      def symbol = Terminal.this.symbol
      override def action =
        Option({ x =>
          f(x.asInstanceOf[L])
        })
      override def reset = Terminal.this.reset
    }
  }

  trait Vertex[+N] extends Symbol[Nothing, N, NoValue] {
    def symbol: org.meerkat.tree.VertexSymbol

    val storedPredicate: (N @uncheckedVariance => Boolean) = (_ => true)

    def ^^ = this.^(identity[N])
    def ^[U](f: N => U) = new SymbolWithAction[Nothing, N, U] {
      def apply(input: Input[Nothing, N], i: Int, sppfLookup: SPPFLookup[Nothing, N]) = Vertex.this(input, i, sppfLookup)
      def name = Vertex.this.name
      def symbol = Vertex.this.symbol
      override def action =
        Option({ x =>
          f(x.asInstanceOf[N])
        })
      override def reset = Vertex.this.reset
    }

    def ::[F <: N @uncheckedVariance](v: Vertex[F]) = {
      val thisPredicate = storedPredicate

      new Vertex[F] {
        override def apply(input: Input[Nothing, F], i: Int, sppfLookup: SPPFLookup[Nothing, F]): CPSResult[NonPackedNode] =
          input.checkNode(i, storedPredicate) match {
            case Some(node) => CPSResult.success(sppfLookup.getVertexNode(node, i))
            case None => CPSResult.failure
          }

        //if-else is necessary because execution should break after first negative result
        override val storedPredicate: F => Boolean =
          n => if (v.storedPredicate(n)) thisPredicate(n) else false

        override def symbol: VertexSymbol = VertexSymbol("label")

        override def name: String = "label"
      }
    }
  }

  def ε = new Terminal[Nothing] {
    def apply(input: Input[Nothing, Nothing], i: Int, sppfLookup: SPPFLookup[Nothing, Nothing]) =
      CPSResult.success(sppfLookup.getEpsilonNode(i))
    def symbol                                                    = TerminalSymbol(name)
    def name                                                      = "epsilon"
    override def toString = name

  }

  // TODO: disallow terminals/nonterminals to be defined as epsilon
  def epsilon = ε

  trait SequenceBuilder[+L, +N, +V]
    extends (Slot => Sequence[L @uncheckedVariance, N @uncheckedVariance])
      with SequenceBuilderOps[L, N,V] {
    import AbstractParser._
    def action: Option[Any => V] = None

    def ~[M >: L, P >: N, U](p: Symbol[M, P, U])(implicit tuple: V |~| U)                = seq(this, p)
    def && = this.&(identity[V])
    def &[U](f: V => U) = new SequenceBuilder[L, N,U] {
      def apply(slot: Slot) = SequenceBuilder.this(slot)
      override def action =
        /*Option ({ x => x match
                            {case g: Set[V] => g.map(i => f(i))
                              case _=> f(x.asInstanceOf[V])}})*/ Option({ x =>
          f(x.asInstanceOf[V])
        })
    }

    def ^[U](f: L => U)(implicit sub: V <:< NoValue) = new SequenceBuilder[L, N,U] {
      def apply(slot: Slot) = SequenceBuilder.this(slot)
      override def action =
        Option({ x =>
          f(x.asInstanceOf[L])
        })
    }

//    var group: Option[AbstractNonterminal[L, N,_]] = None
//    def !(implicit ebnf: EBNF[V]): AbstractNonterminal[L, N,ebnf.Group] = {
//      type T = AbstractNonterminal[L, N,ebnf.Group]
//      group
//        .asInstanceOf[Option[T]]
//        .getOrElse({
//          val p = groupSeq(this); group = Option(p); p
//        })
//    }

    /*
    def ~[U](q: OperatorParsers.AbstractOperatorNonterminal[L, N,U])(implicit tuple: V |~| U) =
      AbstractOperatorParsers.AbstractOperatorParser.seqSeqNt(this, q)(
        OperatorParsers.OperatorImplicits.obj1[L, N, V, U](tuple)
      )
      */
  }

  trait SequenceBuilderWithAction[L, N, +V] extends (Slot => Sequence[L, N])
    with SequenceBuilderOps[L, N, V] {
    import AbstractParser._
    def action: Option[Any => V]
  }

  trait SequenceBuilderOps[+L, +N, +V] extends (Slot => Sequence[L @uncheckedVariance, N @uncheckedVariance]) {
    import AbstractParser._
    def action: Option[Any => V]

    def |[U >: V, M >: L, P >: N](p: AlternationBuilder[M, P,U]) =
      altSeqAlt(this, p.asInstanceOf[AlternationBuilder[L, N, U]])
    def |[U >: V, M >: L, P >: N](p: SequenceBuilder[M, P, U])    =
      altSeq(this, p.asInstanceOf[SequenceBuilder[L, N, U]])
    def |[U >: V, M >: L, P >: N](p: Symbol[M, P, U])             =
      altSeqSym(this, p.asInstanceOf[Symbol[L, N, U]])

    def |[U >: V, M >: L, P >: N](q: SequenceBuilderWithAction[M, P, U]) =
      altSeq(this, q.asInstanceOf[SequenceBuilderWithAction[L, N, U]])
    def |[U >: V, M >: L, P >: N](q: SymbolWithAction[M, P,U])          =
      altSeqSym(this, q.asInstanceOf[SymbolWithAction[L, N, U]])
  }

  trait AlternationBuilder[+L, +N,+V]
    extends (Head[L @uncheckedVariance, N @uncheckedVariance] => Alternation[L  @uncheckedVariance, N  @uncheckedVariance]) {
    import AbstractParser._
    def action: Option[Any => V] = None

    def |[U >: V, M >: L, P >: N](p: AlternationBuilder[M, P ,U]) =
      altAlt(this, p.asInstanceOf[AlternationBuilder[L, N, U]])
    def |[U >: V, M >: L, P >: N](p: SequenceBuilder[M, P,U])    =
      altAltSeq(this, p.asInstanceOf[SequenceBuilder[L, N, U]])
    def |[U >: V, M >: L, P >: N](p: Symbol[M, P, U])             =
      altAltSym(this, p.asInstanceOf[Symbol[L, N, U]])

    def |[U >: V, M >: L, P >: N](q: SequenceBuilderWithAction[M, P,U]) =
      altAltSeq(this, q.asInstanceOf[SequenceBuilderWithAction[L, N, U]])
    def |[U >: V, M >: L, P >: N](q: SymbolWithAction[M, P, U])          =
      altAltSym(this, q.asInstanceOf[SymbolWithAction[L, N, U]])

    //    var group: Option[AbstractNonterminal[L, N,_]] = None
//    def !(implicit ebnf: EBNF[V]): AbstractNonterminal[L, N,ebnf.Group] = {
//      type T = AbstractNonterminal[L, N,ebnf.Group]
//      group.asInstanceOf[Option[T]].getOrElse({ val p = groupAlt(this); group = Option(p); p })
//    }
  }

  trait Symbol[+L, +N, +V] extends AbstractParser[L, N,NonPackedNode]
    with SymbolOps[L, N,V]
    with EBNFs[L, N, V]
//    with CharLevelDisambiguation[L, N, V]
  {
    import AbstractParser._
    def name: String
    def action: Option[Any => V] = None


    def ~[U, M >: L, P >: N](p: Symbol[M, P, U])(implicit tuple: V |~| U)                = seq(this, p)

    def &[U](f: V => U) = new SymbolWithAction[L, N,U] {
      def apply(input: Input[L, N], i: Int, sppfLookup: SPPFLookup[L, N]) = Symbol.this(input, i, sppfLookup)
      def name                                                      = Symbol.this.name; def symbol = Symbol.this.symbol
      override def action =
        Option({ x =>
          f(x.asInstanceOf[V])
        })
      override def reset = Symbol.this.reset
    }
  }

  trait SymbolWithAction[+L, +N, +V] extends Symbol[L, N, V]
    with SymbolOps[L, N, V] {
    import AbstractParser._
    def name: String
  }

  trait SymbolOps[+L, +N,+V] extends AbstractParser[L, N,NonPackedNode] {
    import AbstractParser._
    def name: String
    def action: Option[Any => V]

    def |[U >: V, M >: L, P >: N](p: AlternationBuilder[M, P, U]) = altSymAlt(this, p)
    def |[U >: V, M >: L, P >: N](p: SequenceBuilder[M, P, U])    = altSymSeq(this, p)
    def |[U >: V, M >: L, P >: N](p: Symbol[M, P, U])             = altSym(this, p)

    def |[U >: V, M >: L, P >: N](q: SequenceBuilderWithAction[M, P, U]) = altSymSeq(this, q)
    def |[U >: V, M >: L, P >: N](q: SymbolWithAction[M, P, U])          = altSym(this, q)
  }

  implicit def toTerminal[E](label: E): Terminal[E] =
    terminal(label)

  def terminal[L, N](l: L): Terminal[L] =
    terminal((_: L)== l, l.toString)

  // TODO: fix naming
  def terminal[L, N](p: L => Boolean, termName: String = ""): Terminal[L] = new Terminal[L] {
    private var n: String = ""
    def apply(input: Input[L, Nothing], i: Int, sppfLookup: SPPFLookup[L, Nothing]): CPSResult[TerminalNode[L]] =
      input.filterEdges(i, p) match {
        case edges if edges.isEmpty => CPSResult.failure
        case edges =>
          val terminals = edges.map {
            case (edgeName: L, to: Int) =>
              CPSResult.success(sppfLookup.getTerminalNode(edgeName, i, to))
          }
          terminals.reduceLeft(_.orElse(_))
      }
    
    override def name: String     = termName
    override def symbol           = TerminalSymbol(termName)
    override def toString: String = name.toString
  }

  def E[L](label: L): Terminal[L] =
    terminal(label)

  def E[L](p: L => Boolean): Terminal[L] =
    terminal(p)

  // TODO: fix naming if critical
  def anyE[L]: Terminal[L] = new Terminal[L] {
    override def apply(input: Input[L, Nothing], i: Int, sppfLookup: SPPFLookup[L, Nothing]): CPSResult[TerminalNode[L]] =
      input.filterEdges(i, (_: L) => true) match {
        case edges if edges.isEmpty => CPSResult.failure
        case edges: Seq[(L, Int)] =>
          val terminals = edges.map {
            case (edgeName, to) =>
              CPSResult.success(sppfLookup.getTerminalNode(edgeName.asInstanceOf[L], i, to))
          }
          terminals.reduceLeft(_.orElse(_))
      }
    override def name: String     = "anyE"
    override def symbol           = TerminalSymbol(name)
    override def toString: String = name
  }

  def V[N](l: N): Vertex[N] =
    V((_: N) == l)

  def V[N](p: N => Boolean): Vertex[N] = new Vertex[N]  {
    override def apply(input: Input[Nothing, N], i: Int, sppfLookup: SPPFLookup[Nothing, N]): CPSResult[NonPackedNode] =
      input.checkNode(i, p) match {
        case Some(node) => CPSResult.success(sppfLookup.getVertexNode(node, i))
        case None       => CPSResult.failure
      }

    override val storedPredicate: N => Boolean = p

    override def symbol: VertexSymbol = VertexSymbol("label")
    override def name: String         = "label"
  }

  def ntAlt[L, N, Val](name: String, p: => AlternationBuilder[L, N,Val])            = nonterminalAlt(name, p)
  def ntSeq[L, N, Val](name: String, p: => SequenceBuilder[L, N,Val])               = nonterminalSeq(name, p)
  def ntSym[L, N,Val](name: String, p: => AbstractSymbol[L, N,NonPackedNode, Val]) = nonterminalSym(name, p)

  def notSym[L, N, Val](name: String, p: => AbstractSymbol[L, N,NonPackedNode, Val]) = negativeSym(name, p)

  // TODO: fix
  trait EBNFs[+L, +N, +V] { self: Symbol[L, N, V] =>
    var opt: Option[AbstractNonterminal[_, _ , _]] = None
    def ?[M >: L, P >: N](implicit ebnf: EBNF[V]): AbstractNonterminal[M, P, ebnf.OptOrSeq] = {
      type T = AbstractNonterminal[L, N,ebnf.OptOrSeq]
      opt
        .asInstanceOf[Option[T]]
        .getOrElse({
          val p =
            regular[L, N, NonPackedNode, ebnf.OptOrSeq](org.meerkat.tree.Opt(this.symbol),
              this & ebnf.unit | ε ^ ebnf.empty)
          opt = Option(p)
          p
        }).asInstanceOf[AbstractNonterminal[M, P, ebnf.OptOrSeq]]
    }

    var star: Option[AbstractNonterminal[_, _, _]] = None
    def *[M >: L, P >: N](implicit ebnf: EBNF[V]): AbstractNonterminal[M, P, ebnf.OptOrSeq] = {
      type T = AbstractNonterminal[L, N,ebnf.OptOrSeq]
      star
        .asInstanceOf[Option[T]]
        .getOrElse({
          val p = regular[L, N, NonPackedNode, ebnf.OptOrSeq](
            org.meerkat.tree.Star(this.symbol),
            star.asInstanceOf[Option[T]].get ~ this & ebnf.add | ε ^ ebnf.empty
          )
          star = Option(p)
          p
        }).asInstanceOf[AbstractNonterminal[M, P, ebnf.OptOrSeq]]
    }



    val star_sep: mutable.Map[String, AbstractNonterminal[_, _, _]] = mutable.HashMap.empty
    def *[M >: L, P >: N](sep: Terminal[M])(implicit ebnf: EBNF[V]): AbstractNonterminal[M, P, ebnf.OptOrSeq] = {
      type T = AbstractNonterminal[L, N,ebnf.OptOrSeq]
      star_sep
        .getOrElseUpdate(sep.name, {
          regular[L, N, NonPackedNode, ebnf.OptOrSeq](
            org.meerkat.tree.Star(this.symbol),
            this.+(sep).asInstanceOf[AlternationBuilder[L, N, ebnf.OptOrSeq]] | ε ^ ebnf.empty)
        })
        .asInstanceOf[AbstractNonterminal[M, P, ebnf.OptOrSeq] ]
    }

    var plus: Option[AbstractNonterminal[_, _, _]] = None
    def +[M >: L, P >: N](implicit ebnf: EBNF[V]): AbstractNonterminal[M, P, ebnf.OptOrSeq] = {
      type T = AbstractNonterminal[L, N,ebnf.OptOrSeq]
      plus
        .asInstanceOf[Option[T]]
        .getOrElse({
          val p = regular[L, N, NonPackedNode, ebnf.OptOrSeq](
            org.meerkat.tree.Plus(this.symbol),
            plus.asInstanceOf[Option[T]].get ~ this & ebnf.add | this & ebnf.unit
          )
          plus = Option(p); p
        }).asInstanceOf[AbstractNonterminal[M, P, ebnf.OptOrSeq]]
    }


    val plus_sep: mutable.Map[String, AbstractNonterminal[_, _, _]] = mutable.HashMap.empty
    def +[M >: L, P >: N](sep: M)(implicit ebnf: EBNF[V]): AbstractNonterminal[M, P, ebnf.OptOrSeq] = {
      type T = AbstractNonterminal[L, P, ebnf.OptOrSeq]
      plus_sep
        .getOrElseUpdate(sep.name, {
          regular[M, P, NonPackedNode, ebnf.OptOrSeq](
            org.meerkat.tree.Plus(this.symbol),
            plus_sep(sep.name).asInstanceOf[T] ~ terminal(sep) ~ this & ebnf.add | this & ebnf.unit
          )
        })
        .asInstanceOf[AbstractNonterminal[M, P, ebnf.OptOrSeq]]
    }
  }
//  trait CharLevelDisambiguation[+L, +N, +V] { self: Symbol[L, N, V] =>
//    def \(arg: String) =
//      postFilter(this, (input: Input[String], t: NonPackedNode) => arg != input.substring(t.leftExtent, t.rightExtent), s" \\ $arg")
//    def \(args: Set[String]) =
//      postFilter(
//        this,
//        (input: Input[String], t: NonPackedNode) => !args.contains(input.substring(t.leftExtent, t.rightExtent)),
//        " \\ " + args.mkString(",")
//      )
//    def \(args: String*) =
//      postFilter(
//        this,
//        (input: Input[String], t: NonPackedNode) => !args.contains(input.substring(t.leftExtent, t.rightExtent)),
//        " \\ " + args.mkString(",")
//      )
////    def \(arg: Regex) =
////      postFilter(this, (input, t: NonPackedNode) => !input.matchRegex(arg, t.leftExtent, t.rightExtent), s" \\ $arg")
//    def \(arg: Char) =
//      postFilter(
//        this,
//        (input: Input[String], t: NonPackedNode) => !(t.rightExtent - t.leftExtent == 1 && input.charAt(t.leftExtent) == arg),
//        s" \\ $arg"
//      )
//
//    //def !>>(arg: String) = postFilter(this, (input,t:NonPackedNode) => !input.startsWith(arg, t.rightExtent), s" !>> $arg")
//    //def !>>(args: String*) = postFilter(this, (input,t:NonPackedNode) => !args.exists(input.startsWith(_, t.rightExtent)), " !>> " + args.mkString(","))
////    def !>>(arg: Regex) =
////      postFilter(this, (input, t: NonPackedNode) => input.matchRegex(arg, t.rightExtent) == -1, s" !>> $arg")
//    def !>>(arg: Char) = postFilter(this, (input: Input[String], t: NonPackedNode) => input.charAt(t.rightExtent) != arg, s" !>> $arg")
//
//    def !<<(arg: String) = preFilter(this, (input: Input[String], i) => !input.substring(0, i).endsWith(arg), s"$arg !<< ")
//    def !<<(args: String*) =
//      preFilter(
//        this,
//        (input: Input[String], i) => { val sub = input.substring(0, i); args.filter(sub.endsWith(_)).isEmpty },
//        args.mkString(",") + " !<< "
//      )
////    def !<<(arg: Regex) = preFilter(this, (input, i) => !input.matchRegex(arg, i - 1, i), s"$arg !<< ")
//    def !<<(arg: Char)  = preFilter(this, (input: Input[String], i) => !(i > 0 && input.charAt(i - 1) == arg), s"$arg !<< ")
//  }

}
