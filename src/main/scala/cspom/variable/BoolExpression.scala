package cspom.variable

import cspom.util.IntInterval
import cspom.util.Interval
import cspom.util.Infinitable
import cspom.util.RangeSet
import scala.reflect.runtime.universe._
import IntExpression.implicits._

object BoolExpression extends SimpleExpression.Typed[Boolean] {
  def coerce(e: CSPOMExpression[_]): SimpleExpression[Boolean] = e match {
    case f: FreeVariable   => new BoolVariable()
    case BoolExpression(e) => e
    case e                 => throw new IllegalArgumentException(s"Cannot convert $e to boolean expression")
  }

  def is01(e: CSPOMExpression[_]): Boolean = e match {
    case IntExpression(e) => IntExpression.is01(e)
    case _                => false
  }

  def span(b: SimpleExpression[Boolean]): Interval[Infinitable] = b match {
    case _: BoolVariable      => IntInterval(0, 1)
    case CSPOMConstant(true)  => IntInterval.singleton(1)
    case CSPOMConstant(false) => IntInterval.singleton(0)
    case _                    => throw new IllegalStateException
  }

}