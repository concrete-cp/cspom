package cspom.util

object Interval {
  def unapply[T](itv: Interval[T]): Option[(T, T)] = Some((itv.lb, itv.ub))

  //  def apply[T](lb: T, ub: T): Interval[T] = (lb, ub) match {
  //    case (l: Int, u: Int)                 => IntInterval(l, u).asInstanceOf[Interval[T]]
  //    case (l: Infinitable, u: Infinitable) => IntInterval(l, u).asInstanceOf[Interval[T]]
  //  }

}

trait Interval[@specialized T] {
  def lb: T
  def ub: T
  def isEmpty: Boolean

  def contains(e: T): Boolean

  def &(i: Interval[T]): Interval[T]
  def span(i: Interval[T]): Interval[T]
  def span(lb: T, ub: T): Interval[T]
  def isConnected(i: Interval[T]): Boolean
  def isAfter(i: Interval[T]): Boolean
  def isBefore(i: Interval[T]): Boolean
  def intersects(i: Interval[T]): Boolean

  def lessThan(ub: T): Interval[T]
  def moreThan(lb: T): Interval[T]

  def itvSize: T

}