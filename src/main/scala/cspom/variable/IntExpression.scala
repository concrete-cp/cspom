package cspom.variable

import cspom.util.IntervalsArithmetic.RangeArithmetics
import cspom.util._

import scala.language.implicitConversions

object IntExpression extends SimpleExpression.Typed[Int] {

  def coerce(e: CSPOMExpression[_]): SimpleExpression[Int] = {
    e match {
      case f: FreeVariable => IntVariable.free()
      case IntExpression(e) => e
      case BoolExpression(e) => e.asInstanceOf[SimpleExpression[Int]]
      case _ => throw new IllegalArgumentException(s"Cannot coerce $e to int variable")
    }
  }

  def span(e: SimpleExpression[_]): Interval[Infinitable] = e match {

    case _: FreeVariable => IntInterval.all
    case i: IntVariable => i.domain.span
    case CSPOMConstant(i: Int) => IntInterval.singleton(i)
    case BoolExpression(b) => BoolExpression.span(b)
    case _ => throw new IllegalArgumentException(s"Cannot span $e")
  }

  def is01(e: SimpleExpression[Int]): Boolean = {
    val s = span(e)
    s.lb >= 0 && s.ub <= 1
  }

  def apply(values: Range): SimpleExpression[Int] = apply(
    IntInterval(values.head, values.last))

  def apply(values: IntInterval): SimpleExpression[Int] = apply(RangeSet(values))

  def apply(values: RangeSet[Infinitable]): SimpleExpression[Int] = {
    new ContiguousIntRangeSet(values).singletonMatch match {
      case Some(s) => CSPOMConstant(Math.toIntExact(s))
      case None => IntVariable(values)
    }
  }

  def ofSeq(values: Seq[Int]): SimpleExpression[Int] = values match {
    case Seq(single) => CSPOMConstant(single)
    case values => IntVariable.ofSeq(values)
  }

  def apply(v: Int): SimpleExpression[Int] = CSPOMConstant(v)

  def apply(v0: Int, v: Int*): SimpleExpression[Int] = IntVariable.ofSeq(v0 +: v)

  object implicits {

    implicit def ranges(e: SimpleExpression[_]): RangeSet[Infinitable] = e match {
      case v: IntVariable => v.domain
      case BoolExpression(b) =>
        if (b.contains(false)) {
          if (b.contains(true)) {
            RangeSet(IntInterval(0, 1))
          } else {
            RangeSet(IntInterval.singleton(0))
          }
        } else if (b.contains(true)) {
          RangeSet(IntInterval.singleton(1))
        } else {
          RangeSet.empty
        }

      case CSPOMConstant(i:Int) => RangeSet(IntInterval.singleton(i))

      case e => throw new IllegalStateException
    }

    implicit def arithmetics(e: SimpleExpression[Int]): RangeArithmetics = arithmetics(ranges(e))

    implicit def arithmetics(e: RangeSet[Infinitable]): RangeArithmetics = RangeArithmetics(e)

    implicit def iterable(s: SimpleExpression[_]) =
      new ContiguousIntRangeSet(ranges(s))

  }

  object IntExpression01 {
    def unapply(c: CSPOMExpression[_]): Option[SimpleExpression[Int]] =
      IntExpression.unapply(c).filter(is01)
  }

  object seq {
    def unapply(c: CSPOMExpression[_]): Option[CSPOMSeq[Int]] =
      c match {
        case s: CSPOMSeq[_] =>
          CSPOMSeq.collectAll(s) {
            case IntExpression(e) => e
          }
            .map(intSeq => CSPOMSeq(intSeq, s.definedIndices))
        case _ => None
      }
  }

  object simpleSeq {
    def unapply(c: CSPOMExpression[_]): Option[Seq[SimpleExpression[Int]]] =
      seq.unapply(c).flatMap(SimpleExpression.simpleSeq.unapply)
  }

  object constSeq {
    def unapply(c: CSPOMExpression[_]): Option[Seq[Int]] =
      seq.unapply(c).flatMap(CSPOMConstant.seq.unapply)
  }

}