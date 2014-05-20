package cspom.variable

import scala.collection.JavaConversions
import com.typesafe.scalalogging.slf4j.LazyLogging

final class IntVariable(val domain: IntDomain, params: Map[String, Any] = Map())
  extends CSPOMVariable[Int](params) with LazyLogging {

  if (domain.singleton.isDefined) {
    logger.warn(s"$domain: a variable domain should be of size 2 or more")
  }

  override def toString = s"int variable ($domain)$displayParams"

  def contains[S >: Int](that: S): Boolean = that match {
    case t: Int => domain.contains(t)
    case _ => false
  }

  def intersected(that: SimpleExpression[_ >: Int]): SimpleExpression[Int] =
    that match {
      case CSPOMConstant(c: Int) if domain.contains(c) => CSPOMConstant(c, Map("intersection" -> (this, c)))
      case v: IntVariable => {
        val d = domain.intersect(v.domain)
        d.singleton match {
          case Some(s) => CSPOMConstant(s, Map("intersection" -> (this, v)))
          case None => new IntVariable(d, Map("intersection" -> (this, v)))
        }
      }
      case v: FreeVariable => new IntVariable(domain, Map("intersection" -> (this, v)))
      case t: CSPOMExpression[_] =>
        throw new IllegalArgumentException("Cannot intersect " + this + " with " + t)
    }

  def fullyDefined = domain.fullyDefined

}

object IntVariable {
  def apply(values: Iterable[Int], params: Map[String, Any] = Map()): IntVariable =
    new IntVariable(IntDomain(values), params)

  def free(params: Map[String, Any] = Map()): IntVariable = new IntVariable(FreeInt, params)

  def unapply(v: IntVariable) = Some(v.domain, v.params)
}
