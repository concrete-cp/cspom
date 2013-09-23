package cspom.variable

import cspom.CSPOM

final class IntVariable(name: String, val domain: IntDomain, params: Set[String] = Set())
  extends CSPOMVariable(name, params) {

  def >(other: IntVariable)(implicit problem: CSPOM) = problem.isReified("gt", this, other)

  def >=(other: IntVariable)(implicit problem: CSPOM) = problem.isReified("ge", this, other)

  def <(other: IntVariable)(implicit problem: CSPOM) = problem.isReified("lt", this, other)

  def <=(other: IntVariable)(implicit problem: CSPOM) = problem.isReified("le", this, other)

  def +(other: IntVariable)(implicit problem: CSPOM) = problem.is("add", this, other)

  def -(other: IntVariable)(implicit problem: CSPOM) = problem.is("sub", this, other)

  def *(other: IntVariable)(implicit problem: CSPOM) = problem.is("mul", this, other)

  def /(other: IntVariable)(implicit problem: CSPOM) = problem.is("div", this, other)

  override def toString = s"var $name: Int ($domain)"

  def cspomType = CSPOMInt

}

object IntVariable {
  def of(name: String, values: Seq[Int], params: Set[String] = Set()) =
    new IntVariable(name, IntDomain.of(values: _*), params)

  def valueOf(name: String, valueDesc: String, params: Set[String] = Set()) =
    new IntVariable(name, IntDomain.valueOf(valueDesc), params)

  def free(name: String, params: Set[String] = Set()) = new IntVariable(name, FreeInt, params)
}