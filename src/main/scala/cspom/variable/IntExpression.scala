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

  def is01(e: SimpleExpression[Int]) = implicits.iterable(e).forall(i => i == 0 || i == 1)

  object IntExpression01 extends SimpleExpression.Typed[Int] {
    override def unapply(c: CSPOMExpression[_]): Option[SimpleExpression[Int]] =
      super.unapply(c).filter(is01)
  }

}