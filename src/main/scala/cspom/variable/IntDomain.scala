package cspom.variable

import scala.Ordering
import scala.collection.immutable.SortedSet

import com.typesafe.scalalogging.slf4j.LazyLogging

sealed trait IntDomain extends Iterable[Int] {
  def intersect(domain: IntDomain): IntDomain
  implicit def ordering = Ordering.Int
  def singleton: Option[Int]
  def fullyDefined: Boolean
  def contains(elem: Int): Boolean
}

object IntDomain extends LazyLogging {
  def apply(values: Iterable[Int]) = new IntIntervals(Intervals(values))
  def apply(itv: Interval) = new IntIntervals(Intervals(itv))
}

final case class IntIntervals(intervals: Intervals) extends IntDomain {

  require(intervals.size > 0, "lb <= ub required, and intervals cannot contain more than Int.MaxValue elements.")

  def singleton = if (intervals.size == 1) Some(intervals.head) else None

  def intersect(domain: IntDomain): IntDomain = domain match {
    case FreeInt => this
    case m: IntIntervals => new IntIntervals(m.intervals & intervals)
  }

  def -(elem: Int): IntIntervals = throw new UnsupportedOperationException
  def +(elem: Int): IntIntervals = new IntIntervals(intervals + elem)

  def contains(elem: Int): Boolean = intervals.contains(elem)

  override def toString = intervals.toString

  override def equals(o: Any) = o match {
    case iv: IntIntervals => iv.intervals == intervals
    case _ => super.equals(o)
  }
  def fullyDefined = true

  def iterator = intervals.iterator
}

case object FreeInt extends IntDomain {
  def singleton = None
  def iterator = throw new UnsupportedOperationException

  def intersect(domain: IntDomain) = domain

  def contains(elem: Int) = true

  override def equals(o: Any) = o match {
    case ar: AnyRef => FreeInt eq ar
    case _ => false
  }
  override def toString = "?"
  def fullyDefined = false
}
