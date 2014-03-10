package cspom.variable

import cspom.CSPOM

final class IntVariable(val domain: IntDomain, params: Map[String, Any] = Map())
  extends CSPOMVariable[Int](params) {

  override def toString = s"int variable ($domain)$displayParams"

  def contains[S >: Int](that: S): Boolean = domain.contains(that)

  def intersected(that: SimpleExpression[_ >: Int]): SimpleExpression[Int] =
    that match {
      case c: CSPOMConstant[Int] if domain.contains(c.value) => CSPOMConstant(c.value, c.params ++ params)
      case v: IntVariable => new IntVariable(domain.intersect(v.domain), v.params ++ params)
      case v: FreeVariable => new IntVariable(domain, params ++ v.params)
      case t: CSPOMExpression[_] =>
        throw new IllegalArgumentException("Cannot intersect " + this + " with " + t)
    }
}

object IntVariable {

  def of(values: Int*) = ofSeq(values)

  def ofSeq(values: Seq[Int], params: Map[String, Any] = Map()) =
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
  def ofInterval(lb: Int, ub: Int, params: Map[String, Any]): IntVariable = {
    new IntVariable(new IntInterval(lb, ub), params);
  }

  def ofInterval(lb: Int, ub: Int): IntVariable = {
    ofInterval(lb, ub, Map())
  }

  def free(params: Map[String, Any] = Map()): IntVariable = new IntVariable(FreeInt, params)

  def unapply(v: IntVariable) = Some(v.domain, v.params)
}