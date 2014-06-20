package cspom.variable

import scala.collection.JavaConversions
import com.typesafe.scalalogging.slf4j.LazyLogging
import cspom.util.RangeSet
import com.google.common.collect.ContiguousSet
import com.google.common.collect.DiscreteDomain
import cspom.util.Interval
import cspom.util.Interval.AsOrdered
import cspom.util.IntDiscreteDomain
import com.google.common.math.IntMath
import cspom.util.ContiguousRangeSet
import cspom.util.IntervalsArithmetic
import cspom.util.IntRangeSet
import cspom.util.ContiguousIntRangeSet
import cspom.util.IntInterval

final class IntVariable(val domain: IntRangeSet, params: Map[String, Any] = Map())
  extends CSPOMVariable[Int](params) with LazyLogging {

  val asSortedSet = new ContiguousIntRangeSet(domain)

  def isConvex = domain.isConvex

  if (asSortedSet.singletonMatch.isDefined) {
    logger.warn(s"$domain: a variable domain should be of size 2 or more")
  }

  override def toString = {
    s"int variable ($domain)$displayParams"
  }

  def contains[S >: Int](that: S): Boolean = that match {
    case t: Int => domain.contains(t)
    case _ => false
  }

  def intersected(that: SimpleExpression[_ >: Int]): SimpleExpression[Int] =
    that match {
      case const @ CSPOMConstant(c: Int) if domain.contains(c) =>
        const.asInstanceOf[CSPOMConstant[Int]]
      //CSPOMConstant(c, Map("intersection" -> ((this, c))))
      case v: IntVariable => {
        if (domain == v.domain) {
          v
        } else {
          val d = domain & v.domain

          new ContiguousIntRangeSet(d).singletonMatch match {
            case Some(s) => CSPOMConstant(s) //, Map("intersection" -> ((this, v))))
            case None => new IntVariable(d) //, Map("intersection" -> ((this, v))))
          }
        }
      }
      case v: FreeVariable => this //new IntVariable(domain, Map("intersection" -> ((this, v))))
      case t =>
        throw new IllegalArgumentException("Cannot intersect " + this + " with " + t)
    }

  def fullyDefined = domain.fullyDefined

}

object IntVariable {
  def apply(values: Iterable[Int], params: Map[String, Any] = Map()): IntVariable = {
    new IntVariable(IntRangeSet(values.map(
      v => IntInterval.singleton(v))).canonical, params)
  }

  def apply(values: IntRangeSet): IntVariable = {
    IntVariable(values, Map[String, Any]())
  }

  def apply(values: IntRangeSet, params: Map[String, Any]) =
    new IntVariable(values, params)

  def apply(values: IntInterval): IntVariable = {
    apply(IntRangeSet(values))
  }

  def free(params: Map[String, Any] = Map()): IntVariable =
    new IntVariable(IntRangeSet.all, params)

  def unapply(v: IntVariable) = Some((v.domain, v.params))

  implicit def iterable(s: SimpleExpression[Int]) =
    new ContiguousIntRangeSet(ranges(s))

  implicit def ranges(e: SimpleExpression[Int]): IntRangeSet = e match {
    case v: IntVariable => v.domain
    case CSPOMConstant(c: Int) => IntRangeSet(IntInterval.singleton(c))
  }

  implicit def arithmetics(e: SimpleExpression[Int]) =
    IntervalsArithmetic.RangeArithmetics(ranges(e))

  def intExpression(e: SimpleExpression[_]): SimpleExpression[Int] = e match {
    case f: FreeVariable => free(f.params)
    case i: IntVariable => i
    case c @ CSPOMConstant(_: Int) => c.asInstanceOf[CSPOMConstant[Int]]
    case _ => throw new IllegalArgumentException(s"Cannot convert $e to int variable")
  }
}
