package cspom.variable

import scala.collection.JavaConversions

final class IntVariable(val domain: IntDomain, params: Map[String, Any] = Map())
  extends CSPOMVariable[Int](params) {

  override def toString = s"int variable ($domain)$displayParams"

  def contains[S >: Int](that: S): Boolean = domain.contains(that)

  def intersected(that: SimpleExpression[_ >: Int]): SimpleExpression[Int] =
    that match {
      case c: CSPOMConstant[Int] if domain.contains(c.value) => CSPOMConstant(c.value, Map("intersection" -> (this, c)))
      case v: IntVariable => new IntVariable(domain.intersect(v.domain), Map("intersection" -> (this, v)))
      case v: FreeVariable => new IntVariable(domain, Map("intersection" -> (this, v)))
      case t: CSPOMExpression[_] =>
        throw new IllegalArgumentException("Cannot intersect " + this + " with " + t)
    }
}

object IntVariable {
  def apply(values: Seq[Int], params: Map[String, Any] = Map()): IntVariable =
    new IntVariable(IntDomain(values), params)

  def free(params: Map[String, Any] = Map()): IntVariable = new IntVariable(FreeInt, params)

  def unapply(v: IntVariable) = Some(v.domain, v.params)
}
