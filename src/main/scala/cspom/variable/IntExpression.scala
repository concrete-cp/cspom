package cspom.variable

import scala.language.implicitConversions
import cspom.util.ContiguousIntRangeSet
import cspom.util.Infinitable
import cspom.util.IntInterval
import cspom.util.Interval
import cspom.util.RangeSet
import cspom.util.IntervalsArithmetic.RangeArithmetics
import scala.reflect.runtime.universe._

object IntExpression extends SimpleExpression.Typed[Int] {
  object implicits {

    implicit def ranges(e: SimpleExpression[Int]): RangeSet[Infinitable] = e match {
      case v: IntVariable        => v.domain
      case CSPOMConstant(c: Int) => RangeSet(IntInterval.singleton(c))
      case e                     => throw new IllegalStateException
    }

    implicit def arithmetics(e: SimpleExpression[Int]): RangeArithmetics = arithmetics(ranges(e))

    implicit def arithmetics(e: RangeSet[Infinitable]): RangeArithmetics = RangeArithmetics(e)

    implicit def iterable(s: SimpleExpression[Int]) =
      new ContiguousIntRangeSet(ranges(s))

  }

  def coerce(e: CSPOMExpression[_]): SimpleExpression[Int] = {
    e match {
      case f: FreeVariable  => IntVariable.free()
      case IntExpression(e) => e
      case _                => throw new IllegalArgumentException(s"Cannot coerce $e to int variable")
    }
  }

  def span(e: SimpleExpression[Int]): Interval[Infinitable] = e match {
    case f: FreeVariable       => IntInterval.all
    case i: IntVariable        => i.domain.span
    case CSPOMConstant(i: Int) => IntInterval.singleton(i)
    case _                     => throw new IllegalArgumentException(s"Cannot span $e")
  }

  def is01(e: SimpleExpression[Int]) = e.fullyDefined && implicits.iterable(e).forall(i => i == 0 || i == 1)

  object IntExpression01 {
    def unapply(c: CSPOMExpression[_]): Option[SimpleExpression[Int]] =
      IntExpression.unapply(c).filter(is01)
  }

  def apply(values: Range): SimpleExpression[Int] = apply(
    IntInterval(values.head, values.last))

  def ofSeq(values: Seq[Int]): SimpleExpression[Int] = values match {
    case Seq(single) => CSPOMConstant(single)
    case values      => IntVariable.ofSeq(values)
  }

  def apply(v: Int): SimpleExpression[Int] = CSPOMConstant(v)
  def apply(v0: Int, v: Int*): SimpleExpression[Int] = IntVariable.ofSeq(v0 +: v)

  def apply(values: RangeSet[Infinitable]): SimpleExpression[Int] = {
    new ContiguousIntRangeSet(values).singletonMatch match {
      case Some(s) => CSPOMConstant(s)
      case None    => IntVariable(values)
    }
  }

  def apply(values: IntInterval): SimpleExpression[Int] = apply(RangeSet(values))

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