package cspom.variable

import com.typesafe.scalalogging.LazyLogging
import cspom.util.ContiguousIntRangeSet
import cspom.util.IntInterval
import cspom.util.RangeSet
import cspom.util.IntervalsArithmetic
import cspom.util.Infinitable
import cspom.util.Finite
import cspom.util.Interval

final class IntVariable(val domain: RangeSet[Infinitable], params: Map[String, Any] = Map())
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
    case t: Int => domain.contains(Finite(t))
    case _ => false
  }

  def intersected(that: SimpleExpression[_ >: Int]): SimpleExpression[Int] =
    that match {
      case const @ CSPOMConstant(c: Int) if domain.contains(Finite(c)) =>
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
  def apply(values: Range, params: Map[String, Any] = Map()): IntVariable = apply(
    IntInterval(values.head, values.last), params)

  def ofSeq(values: Seq[Int], params: Map[String, Any]): IntVariable = {
    new IntVariable(RangeSet(values.map(
      v => IntInterval.singleton(v))), params)
  }

  def ofSeq(values: Int*): IntVariable = ofSeq(values, Map[String, Any]())

  def apply(values: RangeSet[Infinitable]): IntVariable = {
    IntVariable(values, Map[String, Any]())
  }

  def apply(values: RangeSet[Infinitable], params: Map[String, Any]) =
    new IntVariable(values, params)

  def apply(values: IntInterval, params: Map[String, Any]): IntVariable = {
    apply(RangeSet(values), params)
  }

  def free(params: Map[String, Any] = Map()): IntVariable =
    new IntVariable(RangeSet.allInt, params)

  def unapply(v: IntVariable) = Some((v.domain, v.params))

  implicit def iterable(s: SimpleExpression[Int]) =
    new ContiguousIntRangeSet(ranges(s))

  implicit def ranges(e: SimpleExpression[Int]): RangeSet[Infinitable] = e match {
    case v: IntVariable => v.domain
    case CSPOMConstant(c: Int) => RangeSet(IntInterval.singleton(c))
  }

  implicit def arithmetics(e: SimpleExpression[Int]) =
    IntervalsArithmetic.RangeArithmetics(ranges(e))

  def intExpression(e: CSPOMExpression[_]): SimpleExpression[Int] = e match {
    case f: FreeVariable => free(f.params)
    case i: IntVariable => i
    case c @ CSPOMConstant(_: Int) => c.asInstanceOf[CSPOMConstant[Int]]
    case _ => throw new IllegalArgumentException(s"Cannot convert $e to int variable")
  }

  def span(e: SimpleExpression[Int]): Interval[Infinitable] = e match {
    case f: FreeVariable => IntInterval.all
    case i: IntVariable => i.domain.span
    case CSPOMConstant(i: Int) => IntInterval.singleton(i)
    case _ => throw new IllegalArgumentException(s"Cannot span $e")
  }
}
