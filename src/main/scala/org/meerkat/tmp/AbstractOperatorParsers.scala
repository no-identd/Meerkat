package org.meerkat.tmp

import org.meerkat.sppf.SPPFLookup
import org.meerkat.util.Input

object Rec extends Enumeration {
  type Rec = Value
  val UNDEFINED, LEFT, RIGHT, BOTH = Value
}

object Assoc extends Enumeration {
  type Assoc = Value
  val UNDEFINED, LEFT, RIGHT, ASSOC, NON_ASSOC = Value
}

object AbstractOperatorParsers {
  
  import AbstractCPSParsers._
  
  type Prec = (Int, Int)
  
  val $: Prec = (0,0)
  
  trait AbstractOperatorParser[+T] extends (Prec => AbstractParser[T]) {
    def nonterminal = false
    def alternation = false
  }
  trait AbstractOperatorSequence[+T] extends ((Prec, Prec) => AbstractParser[T]) {
    def infix = false
    def prefix = false
    def postfix = false
    
    def assoc: Assoc.Assoc = Assoc.UNDEFINED
  }
  
  type Head = AbstractOperatorParser[Any]
  
  trait SequenceBuilder[S <: AbstractOperatorSequence[_]] extends (Head => S)
  trait AlternationBuilder[A <: AbstractOperatorParser[_]] extends ((Head, Group) => (Group => A, Group))
  
  def builderSeq[S <: AbstractOperatorSequence[_]](f: Head => S) 
    = new SequenceBuilder[S] { def apply(head: Head) = f(head) }
  
  def builderAlt[A <: AbstractOperatorParser[_]](f: (Head, Group) => (Group => A, Group))
    = new AlternationBuilder[A] { def apply(head: Head, group: Group) = f(head, group) }
  
  trait CanBuildSequence[A,B] {
    implicit val obj: AbstractCPSParsers.CanBuildSequence[A,B]
    implicit val m1: Memoizable[A];
    implicit val m2: Memoizable[B]
    
    type OperatorSequence <: AbstractOperatorSequence[obj.R]
    
    def infix(f: (Prec, Prec) => obj.Sequence): OperatorSequence
    def postfix(f: (Prec, Prec) => obj.Sequence): OperatorSequence
    def prefix(f: (Prec, Prec) => obj.Sequence): OperatorSequence
    
    def sequence(f: (Prec, Prec) => obj.Sequence): OperatorSequence
    
    def left(p: OperatorSequence): OperatorSequence
  }
  
   trait CanBuildAlternation[A, B >: A] {
    implicit val obj1: AbstractCPSParsers.CanBuildAlternation[A]
    implicit val obj2: AbstractCPSParsers.CanBuildAlternation[B]
    implicit val m1: Memoizable[A]
    implicit val m2: Memoizable[B]
    
    type OperatorAlternation <: AbstractOperatorParser[B]
    def alternation(f: Prec => AbstractParser[B]): OperatorAlternation
  }
  
  object AbstractOperatorParser {
    
    def seq[A,B](p1: AbstractOperatorParser[A], p2: AbstractOperatorParser[B])(implicit builder: CanBuildSequence[A,B]): SequenceBuilder[builder.OperatorSequence] = {
      import builder._
      builderSeq { head =>
        val left = p1 == head; val right = p2 == head
        if (left && right) infix { (prec1, prec2) => AbstractParser.seq(p1(prec1), p2(prec2)) }
        else if (left) postfix { (prec1, prec2) => AbstractParser.seq(p1(prec1), p2(prec2)) }
        else if (right) prefix { (prec1, prec2) => AbstractParser.seq(p1(prec1), p2(prec2)) }
        else sequence { (prec1, prec2) => AbstractParser.seq(p1(prec1), p2(prec2)) }
       }
    }
    
    def seq[A,B,S <: AbstractOperatorSequence[A]](p1: SequenceBuilder[S], p2: AbstractOperatorParser[B])(implicit builder: CanBuildSequence[A,B]): SequenceBuilder[builder.OperatorSequence] = {
      import builder._
      builderSeq { head => val q1 = p1(head)
        val left = q1.infix || q1.postfix; val right = p2 == head
        if (left && right) infix { (prec1, prec2) => AbstractParser.seq(q1(prec1,$), p2(prec2)) }
        else if (left) postfix { (prec1, prec2) => AbstractParser.seq(q1(prec1,$), p2(prec2)) }
        else if (right) prefix { (prec1, prec2) => AbstractParser.seq(q1(prec1,$), p2(prec2)) }
        else sequence { (prec1, prec2) => AbstractParser.seq(q1(prec1,$), p2(prec2)) }
      }
    }
    
    def seq[A,B,S <: AbstractOperatorSequence[A]](p1: SequenceBuilder[S], p2: AbstractParser[B])(implicit builder: CanBuildSequence[A,B]): SequenceBuilder[builder.OperatorSequence] = {
      import builder._
      builderSeq { head => val q1 = p1(head)
        if (q1.infix || q1.postfix) postfix { (prec1, prec2) => AbstractParser.seq(q1(prec1,$), p2) }
        else sequence { (prec1, prec2) => AbstractParser.seq(q1(prec1,$), p2) }
      }
    }
    
    def seq[A,B](p1: AbstractOperatorParser[A], p2: AbstractParser[B])(implicit builder: CanBuildSequence[A,B]): SequenceBuilder[builder.OperatorSequence] = {
      import builder._
      builderSeq { head =>
        if (p1 == head) postfix { (prec1, prec2) => AbstractParser.seq(p1(prec1), p2) }
        else sequence { (prec1, prec2) => AbstractParser.seq(p1(prec1), p2) }
      }
    }
    
    def seq[A,B](p1: AbstractParser[A], p2: AbstractOperatorParser[B])(implicit builder: CanBuildSequence[A,B]): SequenceBuilder[builder.OperatorSequence] = {
      import builder._
      builderSeq { head =>
        if (p2 == head) prefix { (prec1, prec2) => AbstractParser.seq(p1, p2(prec2)) }
        else sequence { (prec1, prec2) => AbstractParser.seq(p1, p2(prec2)) }
      }
    }
    
    def alt[A,B >: A,S1 <: AbstractOperatorParser[A], S2 <: AbstractOperatorParser[B]]
             (p1: AlternationBuilder[S1], p2: AlternationBuilder[S2])(implicit builder: CanBuildAlternation[A,B]): AlternationBuilder[builder.OperatorAlternation] = {
      import builder._
      builderAlt { (head, group1) =>
        val (f2,g2) = p2(head,group1); val (f1,g1) = p1(head,g2)
        ({ group2 => val q1 = f1(group2); val q2 = f2(group2)
             alternation { prec => AbstractParser.alt(q1(prec), q2(prec)) } }, g1)
      }
    }
    
    def alt[A,B >: A,S <: AbstractOperatorParser[A]](p1: AlternationBuilder[S], p2: AbstractOperatorParser[B])(implicit builder: CanBuildAlternation[A,B]): AlternationBuilder[builder.OperatorAlternation] = {
      import builder._
      builderAlt { (head, group1) =>
        val (f1,g1) = p1(head,group1)
        ({ group2 => val q1 = f1(group2)
             alternation { prec => AbstractParser.alt(q1(prec), p2(prec)) } }, g1)
      }
    }
    def alt[A,B >: A,S <: AbstractOperatorParser[B]](p1: AbstractOperatorParser[A], p2: AlternationBuilder[S])(implicit builder: CanBuildAlternation[A,B]): AlternationBuilder[builder.OperatorAlternation] = {
      import builder._
      builderAlt { (head, group1) =>
        val (f2,g2) = p2(head,group1)
        ({ group2 => val q2 = f2(group2)
             alternation { prec => AbstractParser.alt(p1(prec), q2(prec)) } }, g2)
      }
    }
    
    def alt[A,B >: A,S <: AbstractOperatorParser[A]](p1: AlternationBuilder[S], p2: AbstractParser[B])(implicit builder: CanBuildAlternation[A,B]): AlternationBuilder[builder.OperatorAlternation] = {
      import builder._
      builderAlt { (head, group1) =>
        val (f1,g1) = p1(head,group1)
        ({ group2 => val q1 = f1(group2)
             alternation { prec => AbstractParser.alt(q1(prec), p2) } }, g1)
      }
    }
    def alt[A,B >: A,S <: AbstractOperatorParser[B]](p1: AbstractParser[A], p2: AlternationBuilder[S])(implicit builder: CanBuildAlternation[A,B]): AlternationBuilder[builder.OperatorAlternation] = {
      import builder._
      builderAlt { (head, group1) =>
        val (f2,g2) = p2(head,group1)
        ({ group2 => val q2 = f2(group2)
             alternation { prec => AbstractParser.alt(p1, q2(prec)) } }, g2)
      }
    }
    
    def alt[A,B >: A](p1: AbstractOperatorParser[A], p2: AbstractOperatorParser[B])(implicit builder: CanBuildAlternation[A,B]): builder.OperatorAlternation = {
      import builder._
      alternation { prec => AbstractParser.alt(p1(prec), p2(prec)) }
    }
    
    def alt[A,B >: A](p1: AbstractOperatorParser[A], p2: AbstractParser[B])(implicit builder: CanBuildAlternation[A,B]): builder.OperatorAlternation = {
      import builder._
      alternation { prec => AbstractParser.alt(p1(prec), p2) }
    }
    def alt[A,B >: A](p1: AbstractParser[A], p2: AbstractOperatorParser[B])(implicit builder: CanBuildAlternation[A,B]): builder.OperatorAlternation = {
      import builder._
      alternation { prec => AbstractParser.alt(p1, p2(prec)) }
    }
    
    def alt[A,S <: AbstractOperatorSequence[A]](p: SequenceBuilder[S])(implicit builder: CanBuildAlternation[A,A]): AlternationBuilder[builder.OperatorAlternation] = {
      import builder._
      builderAlt { (head, group1) =>
        val q = p(head)
        val (l,g) = if (q.infix || q.prefix || q.postfix) 
                      group1.level(q.assoc, if (q.prefix) -1 else if (q.postfix) 1 else 0)
                    else (-1,group1)
        ({ group2 => alternation(filter(q,l,group2)) }, g) 
      }
    }
    
    /**
     * If |> is used inside an associativity group, it is ignored, i.e., is equivalent to use of |.
     */
    def greater[A,B >: A,S1 <: AbstractOperatorParser[A], S2 <: AbstractOperatorParser[B]]
             (p1: AlternationBuilder[S1], p2: AlternationBuilder[S2])(implicit builder: CanBuildAlternation[A,B]): AlternationBuilder[builder.OperatorAlternation] = {
      import builder._
      builderAlt { (head, group1) =>
        val (f2,g2) = p2(head,group1)
        if (group1 subgroup) {
          val (f1,g1) = p1(head,g2)
          ({ group2 => alternation { prec => AbstractParser.alt(f1(group2)(prec), f2(group2)(prec)) } }, g1)
        } else {
          val (f1,g1) = p1(head,g2.group)
          ({ group2 => val q1 = f1(group2); val q2 = f2(g2.close)
               alternation { prec => AbstractParser.alt(q1(prec), q2(prec)) } }, g1)
        }
      }
    }
    
    def left[A,S <: AbstractOperatorParser[A]](p: AlternationBuilder[S])(implicit builder: CanBuildAlternation[A,A]): AlternationBuilder[S] = {
      import builder._
      builderAlt { (head,group1) =>
        val (f,sg) = p(head, Subgroup(Assoc.LEFT,group1)); val max = sg.max 
        ({ group2 => f(sg.close.asInstanceOf[Subgroup].update(group2))}, group1.update(max)) 
      }
    }
    
    def filter[A](p: AbstractOperatorSequence[A], l: Int, group: Group): Prec => AbstractParser[A] = {
      println(s"Sequence with level: $l, group: $group, assoc: ${p.assoc}")
      if (l == -1) return prec => p(prec, prec)
      val cond: Prec => Boolean = if (!group.subgroup) { 
                                    if (p.infix)       prec => group.max >= prec._1 && group.max >= prec._2
                                    else if (p.prefix) prec => group.max >= prec._1
                                    else               prec => group.max >= prec._2
                                  } else {
                                    if (p.infix)       prec => group.parent.max >= prec._1 && group.parent.max >= prec._2
                                    else if (p.prefix) prec => group.parent.max >= prec._1
                                    else               prec => group.parent.max >= prec._2
                                  }
      if (!group.climb(l)) {
        val extra: Prec => Boolean = 
          if (!group.subgroup) {
            if (l == group.undef) prec => true
            else if (p.infix) {
              p.assoc match {
                case Assoc.LEFT => prec => prec._2 != l
                case Assoc.RIGHT => prec => prec._1 != l
                case Assoc.NON_ASSOC => prec => prec._1 != l && prec._2 != l
              }  
            } else if (p.prefix) {
              p.assoc match {
                case Assoc.NON_ASSOC => prec => prec._2 != l
              }
            } else {
              p.assoc match {
                case Assoc.NON_ASSOC => prec => prec._1 != l
              }
            }
          } else {
            val sg = group.asInstanceOf[Subgroup] 
            if (p.infix)
              sg.assoc match {
                case Assoc.LEFT =>  if (l == sg.undef) prec => !(sg.min <= prec._2 && prec._2 <= sg.max)
                                    else p.assoc match {
                                      case Assoc.RIGHT => prec => prec._1 != l && (prec._2 == l || !(sg.min <= prec._2 && prec._2 <= sg.max))
                                      case Assoc.NON_ASSOC => prec => prec._1 != l && !(sg.min <= prec._2 && prec._2 <= sg.max)
                                      case _ => throw new RuntimeException("Ups, this should not have happened!")
                                    }
                case Assoc.RIGHT => if (l == sg.undef) prec => !(sg.min <= prec._1 && prec._1 <= sg.max)
                                    else p.assoc match {
                                      case Assoc.LEFT => prec => prec._2 != l && (prec._1 == l || !(sg.min <= prec._1 && prec._1 <= sg.max))
                                      case Assoc.NON_ASSOC => prec => prec._2 != l && !(sg.min <= prec._1 && prec._1 <= sg.max)
                                      case _ => throw new RuntimeException("Ups, this should not have happened!")
                                    }
                case Assoc.NON_ASSOC => if (l == sg.undef) prec => !(sg.min <= prec._1 && prec._1 <= sg.max) && !(sg.min <= prec._2 && prec._2 <= sg.max)
                                        else p.assoc match {
                                          case Assoc.LEFT => prec => (prec._1 == l || !(sg.min <= prec._1 && prec._1 <= sg.max)) && !(sg.min <= prec._2 && prec._2 <= sg.max)
                                          case Assoc.RIGHT => prec => !(sg.min <= prec._1 && prec._1 <= sg.max) && (prec._2 == l || !(sg.min <= prec._2 && prec._2 <= sg.max))
                                          case _ => throw new RuntimeException("Ups, this should not have happened!")
                                        }
              }
            else if (p.prefix)
              sg.assoc match {
                case Assoc.LEFT => if (l == sg.undef) prec => true
                                   else prec => prec._2 != l // NON_ASSOC case
                case Assoc.RIGHT => if (l == sg.undef) prec => !(sg.min <= prec._1 && prec._1 <= sg.max)
                                    else prec => prec._2 != l && !(sg.min <= prec._1 && prec._1 <= sg.max) // NON_ASSOC case
                case Assoc.NON_ASSOC => prec => !(sg.min <= prec._1 && prec._1 <= sg.max) && !(sg.min <= prec._2 && prec._2 <= sg.max)
              }
            else
              sg.assoc match {
                case Assoc.LEFT => if (l == sg.undef) prec => !(sg.min <= prec._2 && prec._2 <= sg.max)
                                   else prec => prec._1 != l && !(sg.min <= prec._2 && prec._2 <= sg.max) // NON_ASSOC case
                case Assoc.RIGHT => if (l == sg.undef) prec => true
                                    else prec => prec._1 != l // NON_ASSOC case
                case Assoc.NON_ASSOC => prec => !(sg.min <= prec._1 && prec._1 <= sg.max) && !(sg.min <= prec._2 && prec._2 <= sg.max)
              }
          }
        
          if (group.below.prefix && group.below.postfix)
            return prec => if (cond(prec) && extra(prec)) p((l,prec._2), (prec._1,l)) else FAIL
          else if (group.below.prefix)
            return prec => if (cond(prec) && extra(prec)) p((l,l), (prec._1,l)) else FAIL
          else if (group.below.postfix)
            return prec => if (cond(prec) && extra(prec)) p((l,prec._2), (l,l)) else FAIL
          else 
            return prec => if (cond(prec) && extra(prec)) p((l,l), (l,l)) else FAIL
        
        } else {
          if (!group.subgroup || (group.subgroup && group.min == group.max)) {
            (if (!group.subgroup) p.assoc else group.assoc) match {
              case Assoc.UNDEFINED =>
                return if (group.below.prefix && group.below.postfix) prec => if (cond(prec)) p((l,prec._2), (prec._1,l)) else FAIL
                       else if (group.below.prefix) prec => if (cond(prec)) p((l,l), (prec._1,l)) else FAIL
                       else if (group.below.postfix) prec => if (cond(prec)) p((l,prec._2), (l,l)) else FAIL
                       else prec => if (cond(prec)) p((l,l), (l,l)) else FAIL
              case Assoc.LEFT => 
                return if (group.below.prefix && group.below.postfix) prec => if (cond(prec)) p((l,prec._2), (prec._1,l+1)) else FAIL
                       else if (group.below.prefix) prec => if (cond(prec)) p((l,l), (prec._1,l+1)) else FAIL
                       else if (group.below.postfix) prec => if (cond(prec)) p((l,prec._2), (l+1,l+1)) else FAIL
                       else prec => if (cond(prec)) p((l,l), (l+1,l+1)) else FAIL
              case Assoc.RIGHT => 
                return if (group.below.prefix && group.below.postfix) prec => if (cond(prec)) p((l+1,prec._2), (prec._1,l)) else FAIL
                       else if (group.below.prefix) prec => if (cond(prec)) p((l+1,l+1), (prec._1,l)) else FAIL
                       else if (group.below.postfix) prec => if (cond(prec)) p((l+1,prec._2), (l,l)) else FAIL
                       else prec => if (cond(prec)) p((l+1,l+1), (l,l)) else FAIL
              case Assoc.NON_ASSOC => 
                // TODO: extra condition for unary operators (non-assoc group that has unary cannot climb !!!)
                return if (group.below.prefix && group.below.postfix) prec => if (cond(prec)) p((l+1,prec._2), (prec._1,l+1)) else FAIL
                       else if (group.below.prefix) prec => if (cond(prec)) p((l+1,l+1), (prec._1,l+1)) else FAIL
                       else if (group.below.postfix) prec => if (cond(prec)) p((l+1,prec._2), (l+1,l+1)) else FAIL
                       else prec => if (cond(prec)) p((l+1,l+1), (l+1,l+1)) else FAIL
            } 
          } else {
            // TODO: for each level that is not equal to undef, add extra unequality constraints
            ???
          }
      }
    }
    
    object FAIL extends AbstractParser[Nothing] { def apply(input: Input, i: Int, sppfLookup: SPPFLookup) = CPSResult.failure }
    
  }
  
  class Unary(val prefix: Boolean, val postfix: Boolean) { override def toString = s"unary($prefix, $postfix)" }
  
  object Unary { def apply() = new Unary(false, false) }
  
  class Group(val assoc: Assoc.Assoc, val min: Int, val max: Int, val undef: Int, 
              val here: Unary, val below: Unary) {
    
    def subgroup = false
    def parent = this
    
    def group = new Group(assoc, max, max, -1, Unary(), new Unary(here.prefix || below.prefix, here.postfix || below.postfix))
    
    def update(max: Int) = new Group(assoc, min, max, undef, here, below)   
    def update(max: Int, undef: Int, here: Unary) = new Group(assoc, min, max, undef, here, below)
    
    def close = if (min == max) this else new Group(assoc, min, max - 1, undef, here, below)
    
    /**
     *  @param unary (-1 to indicate prefix, 1 to indicate postfix)
     *  Note: LEFT and RIGHT associativity have no effect on prefix and postfix operators 
     */
    def level(assoc: Assoc.Assoc, unary: Int = 0): (Int, Group) = {
      var undef = this.undef; var max = this.max
      val level = if (assoc == this.assoc || assoc == Assoc.UNDEFINED 
                        || ((unary == -1 || unary == 1) && assoc != Assoc.NON_ASSOC)) {
                    if (undef != -1) undef else { undef = max; max += 1; undef }
                  } else { max += 1; this.max }
      val here = new Unary(if (unary == -1) true else this.here.prefix, 
                           if (unary == 1) true else this.here.postfix)
      (level, this.update(max, undef, here))
    }
    
    def climb(level: Int) = if (min == max) true
                            else false
                            
    override def toString = s"Group($assoc, $min, $max, $undef, $here, $below)"
    
  }
  
  object Group {
    def apply() = new Group(Assoc.UNDEFINED,1,1,-1,Unary(),Unary())
  }
  
  class Subgroup(override val assoc: Assoc.Assoc, override val min: Int, override val max: Int, override val undef: Int,
                 override val here: Unary, override val below: Unary,
                 override val parent: Group) extends Group(assoc, min, max, undef, here, below) {
    
    override def subgroup = true
    
    override def group = new Subgroup(assoc, max, max, undef, 
                           Unary(), new Unary(here.prefix || below.prefix, here.postfix || below.postfix), 
                           parent)
    
    override def update(max: Int) = new Subgroup(assoc, min, max, undef, here, below, parent)    
    override def update(max: Int, undef: Int, here: Unary) = new Subgroup(assoc, min, max, undef, here, below, parent)
    
    override def close = if (min == max) this else new Subgroup(assoc, min, max - 1, undef, here, below, parent)
    
    def update(parent: Group) = new Subgroup(assoc, min, max, undef, here, below, parent)
    
    override def climb(level: Int) = {
      if (this.min == parent.min && this.max == parent.max 
            && level == this.undef)
        true
      else false
    }
    
    override def toString = s"Subgroup($assoc,$min,$max,$undef,$here,$below,$parent)"
    
  } 
  
  object Subgroup {
    def apply(assoc: Assoc.Assoc, parent: Group) = new Subgroup(assoc, parent.max, parent.max, -1, parent.here, parent.below, parent)
  }
  
}