package cspom.util

object Interval {
  def unapply[T](itv: Interval[T]): Option[(T, T)] = Some((itv.lb, itv.ub))
}

abstract class Interval[T: Ordering] {
  private val ordering = implicitly[Ordering[T]]

  import ordering.mkOrderingOps

  def lb: T

  def ub: T

  def isEmpty: Boolean

  def contains(e: T): Boolean = lb <= e && e <= ub

  def &(i: Interval[T]): Interval[T]

  def span(i: Interval[T]): Interval[T]

  def span(lb: T, ub: T): Interval[T]

  def isConnected(i: Interval[T]): Boolean =
    !(this isAfter i) && !(this isBefore i)

  def isAfter(i: Interval[T]): Boolean

  def isBefore(i: Interval[T]): Boolean

  def intersects(i: Interval[T]): Boolean = lb <= i.ub && ub >= i.lb

  def lessThan(ub: T): Interval[T]

  def moreThan(lb: T): Interval[T]

  def itvSize: T

  override def hashCode: Int = {
    41 * lb.hashCode + ub.hashCode
  }

  override def equals(o: Any): Boolean = o match {
    case i: Interval[_] => i.lb == lb && i.ub == ub
    case _ => false
  }

}

class IntervalOrdering[@specialized T] extends Ordering[Interval[T]] {
  def compare(i: Interval[T], j: Interval[T]): Int = {
    if (i isAfter j) {
      1
    } else if (i isBefore j) {
      -1
    } else {
      0
    }
  }
}