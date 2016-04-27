package cspom.util

object Interval {
  def unapply[T](itv: Interval[T]): Option[(T, T)] = Some((itv.lb, itv.ub))

  //  def apply[T](lb: T, ub: T): Interval[T] = (lb, ub) match {
  //    case (l: Int, u: Int)                 => IntInterval(l, u).asInstanceOf[Interval[T]]
  //    case (l: Infinitable, u: Infinitable) => IntInterval(l, u).asInstanceOf[Interval[T]]
  //  }

}

abstract class Interval[T <% Ordered[T]] {
  def lb: T
  def ub: T
  def isEmpty: Boolean

  def contains(e: T): Boolean =
    lb <= e && e <= ub

  def &(i: Interval[T]): Interval[T]
  def span(i: Interval[T]): Interval[T]
  def span(lb: T, ub: T): Interval[T]
  def isConnected(i: Interval[T]): Boolean =
    !(this isAfter i) && !(this isBefore i)

  def isAfter(i: Interval[T]): Boolean
  def isBefore(i: Interval[T]): Boolean
  def intersects(i: Interval[T]): Boolean =
    lb <= i.ub && ub >= i.lb

  def lessThan(ub: T): Interval[T]
  def moreThan(lb: T): Interval[T]

  def itvSize: T

  override def hashCode = {
    41 * lb.hashCode + ub.hashCode
  }

}

class IntervalOrdering[@specialized T] extends Ordering[Interval[T]] {
  def compare(i: Interval[T], j: Interval[T]) = {
    if (i isAfter j) {
      1
    } else if (i isBefore j) {
      -1
    } else {
      0
    }
  }
}