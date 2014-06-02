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

final class IntVariable(val domain: RangeSet[Int], params: Map[String, Any] = Map())
  extends CSPOMVariable[Int](params) with LazyLogging {

  val asSortedSet = new ContiguousRangeSet(domain, IntDiscreteDomain)

  def isConvex = domain.isConvex

  if (asSortedSet.singletonMatch.isDefined) {
    logger.warn(s"$domain: a variable domain should be of size 2 or more")
  }

  override def toString = {
    val c = domain.canonical(IntDiscreteDomain)
    s"int variable ($c)$displayParams"
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
        new ContiguousRangeSet(d, IntDiscreteDomain).singletonMatch match {
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
      v => Interval.singleton(v))).canonical(IntDiscreteDomain), params)
  }

  def apply(values: RangeSet[Int]): IntVariable = {
    IntVariable(values, Map[String, Any]())
  }

  def apply(values: RangeSet[Int], params: Map[String, Any]) = new IntVariable(values, params)

  def apply(values: Interval[Int]): IntVariable = {
    apply(RangeSet(values))
  }

  def free(params: Map[String, Any] = Map()): IntVariable = new IntVariable(RangeSet.all[Int], params)

  def unapply(v: IntVariable) = Some((v.domain, v.params))

  implicit def iterable(s: SimpleExpression[Int]) = new ContiguousRangeSet(ranges(s), IntDiscreteDomain)

  implicit def ranges(e: SimpleExpression[Int]): RangeSet[Int] = e match {
    case v: IntVariable => v.domain
    case CSPOMConstant(c: Int) => RangeSet(Interval.singleton(c))
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
