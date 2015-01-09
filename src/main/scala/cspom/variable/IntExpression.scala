package cspom.variable

import scala.language.implicitConversions

import cspom.util.ContiguousIntRangeSet
import cspom.util.Infinitable
import cspom.util.IntInterval
import cspom.util.Interval
import cspom.util.RangeSet
import cspom.util.IntervalsArithmetic.RangeArithmetics

object IntExpression {
  object implicits {

    implicit def ranges(e: SimpleExpression[Int]): RangeSet[Infinitable] = e match {
      case v: IntVariable        => v.domain
      case CSPOMConstant(c: Int) => RangeSet(IntInterval.singleton(c))
    }

    implicit def arithmetics(e: SimpleExpression[Int]): RangeArithmetics = arithmetics(ranges(e))

    implicit def arithmetics(e: RangeSet[Infinitable]): RangeArithmetics = RangeArithmetics(e)

    implicit def iterable(s: SimpleExpression[Int]) =
      new ContiguousIntRangeSet(ranges(s))

  }

  def apply(e: CSPOMExpression[_]): SimpleExpression[Int] = e match {
    case f: FreeVariable           => IntVariable.free(f.params)
    case i: IntVariable            => i
    case c @ CSPOMConstant(_: Int) => c.asInstanceOf[CSPOMConstant[Int]]
    case _                         => throw new IllegalArgumentException(s"Cannot convert $e to int variable")
  }

  def isInt(e: CSPOMExpression[_]): Boolean = e match {
    case i: IntVariable            => true
    case c @ CSPOMConstant(_: Int) => true
    case _                         => false
  }

  def span(e: SimpleExpression[Int]): Interval[Infinitable] = e match {
    case f: FreeVariable       => IntInterval.all
    case i: IntVariable        => i.domain.span
    case CSPOMConstant(i: Int) => IntInterval.singleton(i)
    case _                     => throw new IllegalArgumentException(s"Cannot span $e")
  }
}