package cspom.variable

import cspom.CSPOM

final class IntVariable(val domain: IntDomain, params: Set[String] = Set())
  extends CSPOMVariable(params) with IntExpression {

  override def toString = s"int variable ($domain)"

  def contains(that: CSPOMConstant) = domain.contains(that)

  def intersected(that: CSPOMExpression): CSPOMExpression = that match {
    case IntConstant(v) => IntVariable.ofSeq(Seq(v), params)
    case v: IntVariable => new IntVariable(domain.intersect(v.domain), params)
    case v: FreeVariable => this
    case t: CSPOMExpression => throw new IllegalArgumentException("Cannot intersect " + this + " with " + t)
  }
}

object IntVariable {
  
  def of(values: Int*) = ofSeq(values)
  
  def ofSeq(values: Seq[Int], params: Set[String] = Set()) =
    new IntVariable(IntDomain.of(values: _*), params)

  /**
   * Constructs a new variable with a domain defined by lower
   * and upper bounds.
   *
   * @param <E>
   *            Type of bounds.
   * @param lB
   *            Lower bound of the domain
   * @param uB
   *            Upper bound of the domain
   */
  def ofInterval(lb: Int, ub: Int, params: Set[String] = Set()) = {
    new IntVariable(new IntInterval(lb, ub), params);
  }

  def free(params: String*): IntVariable = free(params.toSet)
  def free(params: Set[String]): IntVariable = new IntVariable(FreeInt, params)

  def unapply(v: IntVariable) = Some(v.domain, v.params)
}