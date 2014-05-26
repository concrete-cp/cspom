package cspom.variable

import scala.collection.JavaConversions
import com.typesafe.scalalogging.slf4j.LazyLogging
import cspom.util.RangeSet
import com.google.common.collect.ContiguousSet
import com.google.common.collect.DiscreteDomain
import cspom.util.GuavaRange
import cspom.util.GuavaRange.AsOrdered
import cspom.util.IntDiscreteDomain
import com.google.common.math.IntMath

final class IntVariable(val domain: RangeSet[Int], params: Map[String, Any] = Map())
  extends CSPOMVariable[Int](params) with LazyLogging {

  implicit def domainType = IntDiscreteDomain

  def domainValues = domain.allValues.toIterable

  if (domain.singletonMatch.isDefined) {
    logger.warn(s"$domain: a variable domain should be of size 2 or more")
  }

  override def toString = {
    val c = domain.canonical
    val d = c & GuavaRange.closed(c.lowerBound, IntMath.checkedSubtract(c.upperBound, 1))
    s"int variable ($d)$displayParams"
  }

  def contains[S >: Int](that: S): Boolean = that match {
    case t: Int => domain.contains(t)
    case _ => false
  }

  def intersected(that: SimpleExpression[_ >: Int]): SimpleExpression[Int] =
    that match {
      case CSPOMConstant(c: Int) if domain.contains(c) =>
        CSPOMConstant(c, Map("intersection" -> ((this, c))))
      case v: IntVariable => {
        val d = domain & v.domain
        d.singletonMatch match {
          case Some(s) => CSPOMConstant(s, Map("intersection" -> ((this, v))))
          case None => new IntVariable(d, Map("intersection" -> ((this, v))))
        }
      }
      case v: FreeVariable => new IntVariable(domain, Map("intersection" -> ((this, v))))
      case t: CSPOMExpression[_] =>
        throw new IllegalArgumentException("Cannot intersect " + this + " with " + t)
    }

  def fullyDefined = domain.fullyDefined

}

object IntVariable {
  def apply(values: Iterable[Int], params: Map[String, Any] = Map()): IntVariable = {
    new IntVariable(RangeSet(values.map(
      v => GuavaRange.singleton(v))).canonical(IntDiscreteDomain), params)
  }

  def apply(values: RangeSet[Int]): IntVariable = {
    new IntVariable(values, Map())
  }

  def apply(values: GuavaRange[Int]): IntVariable = {
    apply(RangeSet(values))
  }

  def free(params: Map[String, Any] = Map()): IntVariable = new IntVariable(RangeSet.all[Int], params)

  def unapply(v: IntVariable) = Some((v.domain, v.params))
}
