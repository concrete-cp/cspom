package cspom.variable

import cspom.CSPOM

final class BoolVariable(name: String, params: Set[String] = Set())
  extends CSPOMVariable(name, params) with BoolExpression {

  override def toString = s"var $name: Boolean"

  def cspomType = CSPOMBool
}