package cspom.variable

import cspom.CSPOM

final class IntVariable(val domain: IntDomain, params: Set[Any] = Set())
  extends CSPOMVariable[Int](params) {

  override def toString = s"int variable ($domain)"

  def contains[S >: Int](that: S): Boolean = domain.contains(that)

  def intersected(that: SimpleExpression[_ >: Int]): SimpleExpression[Int] = that match {
    case CSPOMConstant(v: Int) => IntVariable.ofSeq(Seq(v), params)
    case v: IntVariable => new IntVariable(domain.intersect(v.domain), params)
    case v: FreeVariable => this
    case t: CSPOMExpression[_] => throw new IllegalArgumentException("Cannot intersect " + this + " with " + t)
  }
}

object IntVariable {

  def of(values: Int*) = ofSeq(values)

  def ofSeq(values: Seq[Int], params: Set[Any] = Set()) =
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
  def ofInterval(lb: Int, ub: Int, params: Set[Any] = Set()) = {
    new IntVariable(new IntInterval(lb, ub), params);
  }

  def free(params: Any*): IntVariable = free(params.toSet)
  def free(params: Set[Any]): IntVariable = new IntVariable(FreeInt, params)

  def unapply(v: IntVariable) = Some(v.domain, v.params)
}