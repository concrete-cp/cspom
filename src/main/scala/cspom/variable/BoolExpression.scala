package cspom.variable

import cspom.util.IntInterval
import cspom.util.Interval
import cspom.util.Infinitable

object BoolExpression {
  def apply(e: CSPOMExpression[_]): SimpleExpression[Boolean] = e match {
    case f: FreeVariable               => new BoolVariable()
    case b: BoolVariable               => b
    case c @ CSPOMConstant(_: Boolean) => c.asInstanceOf[CSPOMConstant[Boolean]]
    case e                             => throw new IllegalArgumentException(s"Cannot convert $e to boolean expression")
  }

  def isBool(e: CSPOMExpression[_]): Boolean = e match {
    case b: BoolVariable           => true
    case CSPOMConstant(_: Boolean) => true
    case _                         => false
  }

  def span(b: SimpleExpression[Boolean]): Interval[Infinitable] = b match {
    case _: BoolVariable      => IntInterval(0, 1)
    case CSPOMConstant(true)  => IntInterval.singleton(1)
    case CSPOMConstant(false) => IntInterval.singleton(0)
    case _                    => throw new IllegalStateException
  }

}