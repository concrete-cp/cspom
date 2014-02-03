package cspom.variable

import cspom.CSPOM

final class BoolVariable(params: Set[String] = Set())
  extends CSPOMVariable(params) with BoolExpression {
  def this(params: String*) = this(params.toSet)

  override def toString = s"boolean variable"

  def intersected(that: CSPOMExpression) = that match {
    case t: BoolVariable => this
    case t: BoolExpression with CSPOMConstant => t
    case _ => throw new IllegalArgumentException
  }

  def contains(that: CSPOMConstant) = that == CSPOMTrue || that == CSPOMFalse

  def neg = ???
}