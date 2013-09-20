package cspom.variable

import cspom.CSPOM

final class BoolVariable(name: String, params: Set[String] = Set())
  extends CSPOMVariable(name, params) {
  def |(other: BoolVariable)(implicit problem: CSPOM) = problem.isReified("or", this, other)

  def &(other: BoolVariable)(implicit problem: CSPOM) = problem.isReified("and", this, other)

  override def toString = s"var $name: Boolean"

  def cspomType = CSPOMBool
}