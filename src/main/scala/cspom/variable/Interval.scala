package cspom.variable

import scala.collection.SortedSet

object Interval {
  def apply(lb: Int, ub: Int) = new Interval(lb, ub)

  def unapply(i: Interval): Option[(Int, Int)] = if (i.nonEmpty) Some(i.lb, i.ub) else None
}

final class Interval(private val _lb: Int, private val _ub: Int) {

  val size: Int = {
    val l = 1l + _ub - _lb
    require(l <= Int.MaxValue)
    math.max(0l, l).toInt
  }

  def ub: Int = {
    require(nonEmpty)
    _ub
  }

  def lb: Int = {
    require(nonEmpty)
    _lb
  }

  def contains(v: Int) = _lb <= v && v <= _ub

  def range = _lb to _ub

  def isEmpty = size == 0

  def nonEmpty = size > 0

  override def equals(o: Any) = o match {
    case i: Interval =>
      if (isEmpty) {
        i.isEmpty
      } else {
        i.nonEmpty && i.lb == _lb && i.ub == _ub
      }
    case _ => false
  }

  override def hashCode = _lb + 751 * _ub

  /**
   * [a, b] + [c, d] = [a + c, b + d]
   * [a, b] − [c, d] = [a − d, b − c]
   * [a, b] × [c, d] = [min (a × c, a × d, b × c, b × d), max (a × c, a × d, b × c, b × d)]
   * [a, b] ÷ [c, d] = [min (a ÷ c, a ÷ d, b ÷ c, b ÷ d), max (a ÷ c, a ÷ d, b ÷ c, b ÷ d)] when 0 is not in [c, d].
   */

  def +(i: Interval) = Interval(lb + i.lb, ub + i.ub)

  def +(v: Int) = Interval(lb + v, ub + v)

  def -(i: Interval) = Interval(lb - i.ub, ub - i.lb)

  def -(v: Int) = this + -v

  def *(i: Interval) = {
    val Interval(c, d) = i
    Interval(List(lb * c, lb * d, ub * c, ub * d).min, List(lb * c, lb * d, ub * c, ub * d).max)
  }

  def *(v: Int) = {
    Interval(math.min(lb * v, ub * v), math.max(lb * v, ub * v))
  }

  def /(i: Interval) = {
    if (i.contains(0)) throw new ArithmeticException
    val Interval(c, d) = i
    Interval(
      List(ceilDiv(lb, c), ceilDiv(lb, d), ceilDiv(ub, c), ceilDiv(ub, d)).min,
      List(floorDiv(lb, c), floorDiv(lb, d), floorDiv(ub, c), floorDiv(ub, d)).max)
  }

  def /(v: Int) = {
    if (v >= 0) {
      Interval(ceilDiv(lb, v), floorDiv(ub, v))
    } else {
      Interval(ceilDiv(ub, v), floorDiv(lb, v))
    }

    //    if (l < u) Interval(l, u) else Interval(u, l)
  }

  def floorDiv(dividend: Int, divisor: Int) = {
    val roundedTowardsZeroQuotient = dividend / divisor;
    val dividedEvenly = (dividend % divisor) == 0;
    if (dividedEvenly) {
      roundedTowardsZeroQuotient;
    } else {
      // If they're of opposite sign then we rounded 
      // UP towards zero so we rem one. If they're of the same sign then 
      // we rounded DOWN towards zero, so we are done.

      if (divisor.signum == dividend.signum) {
        roundedTowardsZeroQuotient;
      } else {
        roundedTowardsZeroQuotient - 1;
      }
    }
  }

  def ceilDiv(dividend: Int, divisor: Int) = {

    val roundedTowardsZeroQuotient = dividend / divisor;
    val dividedEvenly = (dividend % divisor) == 0;
    if (dividedEvenly) {
      roundedTowardsZeroQuotient;
    } else {
      // If they're of opposite sign then we rounded 
      // UP towards zero so we're done. If they're of the same sign then 
      // we rounded DOWN towards zero, so we need to add one.

      if (divisor.signum == dividend.signum) {
        roundedTowardsZeroQuotient + 1;
      } else {
        roundedTowardsZeroQuotient;
      }
    }
  }

  def intersect(i: Interval) = {
    val l = math.max(lb, i.lb)
    val u = math.min(ub, i.ub)
    Interval(l, u)
  }

  def intersects(i: Interval): Boolean = (this intersect i).nonEmpty

  def isBefore(i: Interval): Boolean = i.lb.toLong - this.ub > 1

  def isAfter(i: Interval): Boolean = this.lb.toLong - i.ub > 1

  def union(i: Interval) = Interval(math.min(lb, i.lb), math.max(ub, i.ub))

  def negate = Interval(-ub, -lb)

  def abs =
    if (ub < 0) { negate }
    else if (lb > 0) { this }
    else { Interval(0, math.max(-lb, ub)) }
  
  override def toString = if (isEmpty) "[]" else if (lb == ub) s"[$lb]" else s"[$lb..$ub]"

}
