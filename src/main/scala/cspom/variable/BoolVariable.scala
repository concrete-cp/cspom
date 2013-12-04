package cspom.variable

import cspom.CSPOM

final class BoolVariable(name: String, params: Set[String] = Set())
  extends CSPOMVariable(name, params) with BoolExpression {
  def this(name: String, params: String*) = this(name, params.toSet)
  
  override def toString = s"var $name: Boolean"

  def cspomType = CSPOMBool

  def intersected(that: CSPOMExpression) = that match {
    case t: BoolVariable => this
    case t: BoolExpression with CSPOMConstant => t
    case _ => throw new IllegalArgumentException
  }

  def contains(that: CSPOMConstant) = that == CSPOMTrue || that == CSPOMFalse
  
  def neg = ???
}