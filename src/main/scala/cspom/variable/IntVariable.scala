package cspom.variable

import cspom.CSPOM

final class IntVariable(name: String, val domain: IntDomain, params: Set[String] = Set())
  extends CSPOMVariable(name, params) with IntExpression {

  override def toString = s"var $name: Int ($domain)"

  def cspomType = CSPOMInt

  def contains(that: CSPOMConstant) = domain.contains(that)

  def intersected(that: CSPOMExpression): CSPOMExpression = that match {
    case IntConstant(v) => IntVariable.of(name, Seq(v), params)
    case v: IntVariable => IntVariable.of(name, domain.intersect(v.domain), params)
    case _ => throw new IllegalArgumentException
  }
}

object IntVariable {
  def of(name: String, values: Seq[Int], params: Set[String] = Set()) =
    new IntVariable(name, IntDomain.of(values: _*), params)

  def valueOf(name: String, valueDesc: String, params: String*) =
    new IntVariable(name, IntDomain.valueOf(valueDesc), params.toSet)

  def free(name: String, params: String*): IntVariable = free(name, params.toSet)
  def free(name: String, params: Set[String]): IntVariable = new IntVariable(name, FreeInt, params)

  def unapply(v: IntVariable) = Some(v.name, v.domain, v.params)
}