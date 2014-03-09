package cspom.variable

import scala.collection.JavaConversions

final class IntVariable(val domain: IntDomain, params: Set[Any] = Set())
  extends CSPOMVariable[Int](params) {

  override def toString = s"int variable ($domain)" + params.map(" :: " + _).mkString

  def contains[S >: Int](that: S): Boolean = domain.contains(that)

  def intersected(that: SimpleExpression[_ >: Int]): SimpleExpression[Int] =
    that match {
      case c: CSPOMConstant[Int] => CSPOMConstant(c.value, c.params ++ params)
      case v: IntVariable => new IntVariable(domain.intersect(v.domain), v.params ++ params)
      case v: FreeVariable => new IntVariable(domain, params ++ v.params)
      case t: CSPOMExpression[_] =>
        throw new IllegalArgumentException("Cannot intersect " + this + " with " + t)
    }
}

object IntVariable {

  def apply(values: java.util.List[Int]): IntVariable =
    apply(JavaConversions.asScalaBuffer(values))

  def apply(values: Seq[Int], params: Set[Any] = Set()): IntVariable =
    new IntVariable(IntDomain(values), params)

  //  /**
  //   * Constructs a new variable with a domain defined by lower
  //   * and upper bounds.
  //   *
  //   * @param <E>
  //   *            Type of bounds.
  //   * @param lB
  //   *            Lower bound of the domain
  //   * @param uB
  //   *            Upper bound of the domain
  //   */
  //  def ofInterval(lb: Int, ub: Int, params: Set[Any] = Set()) = {
  //    new IntVariable(new IntInterval(lb, ub), params);
  //  }

  def free(params: Any*): IntVariable = free(params.toSet)
  def free(params: Set[Any]): IntVariable = new IntVariable(FreeInt, params)

  def unapply(v: IntVariable) = Some(v.domain, v.params)
}