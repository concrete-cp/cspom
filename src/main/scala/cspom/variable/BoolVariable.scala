package cspom.variable

import cspom.CSPOM

final class BoolVariable(params: Set[Any] = Set())
  extends CSPOMVariable[Boolean](params) {
  def this(params: Any*) = this(params.toSet)

  override def toString = s"boolean variable"

  def intersected(that: SimpleExpression[_ >: Boolean]): SimpleExpression[Boolean] = that match {
    case t: BoolVariable => this
    case t: CSPOMConstant[Boolean] => t
    case _ => throw new IllegalArgumentException
  }

  def contains[S >: Boolean](that: S) = that == CSPOMTrue || that == CSPOMFalse

  def neg = ???
}