package cspom.variable

import scala.Ordering
import scala.collection.immutable.SortedSet

import com.typesafe.scalalogging.slf4j.LazyLogging

sealed trait IntDomain extends SortedSet[Int] {
  def intersect(domain: IntDomain): IntDomain
  implicit def ordering = Ordering.Int
  def singleton: Boolean
  def fullyDefined: Boolean
}

object IntDomain extends LazyLogging {
  def apply(values: Iterable[Int]) = new IntIntervals(Intervals(values))
  def apply(itv: Interval) = new IntIntervals(Intervals(itv))
}

final case class IntIntervals(intervals: Intervals) extends IntDomain {

  require(size > 0, "lb <= ub required, and intervals cannot contain more than Int.MaxValue elements.")

  override def size = intervals.size

  def singleton = size == 1

  def intersect(domain: IntDomain): IntDomain = domain match {
    case FreeInt => this
    case m: IntIntervals => new IntIntervals(m.intervals & intervals)
  }

  def -(elem: Int): IntIntervals = throw new UnsupportedOperationException
  def +(elem: Int): IntIntervals = new IntIntervals(intervals + elem)

  def contains(elem: Int): Boolean = intervals.contains(elem)
  //implicit def ordering: Ordering[Int] = throw new UnsupportedOperationException
  def rangeImpl(from: Option[Int], until: Option[Int]): SortedSet[Int] = throw new UnsupportedOperationException
  def iterator: Iterator[Int] = intervals.iterator
  def keysIteratorFrom(start: Int): Iterator[Int] = ???
  override def toString = intervals.toString

  override def equals(o: Any) = o match {
    case iv: IntIntervals => iv.intervals == intervals
    case _ => super.equals(o)
  }
  def fullyDefined = true
}

case object FreeInt extends IntDomain {
  def singleton = false
  def -(elem: Int): SortedSet[Int] = throw new UnsupportedOperationException
  def +(elem: Int): SortedSet[Int] = throw new UnsupportedOperationException
  def contains(elem: Int): Boolean = elem.isInstanceOf[Int]
  def rangeImpl(from: Option[Int], until: Option[Int]): SortedSet[Int] = throw new UnsupportedOperationException

  def intersect(domain: IntDomain) = domain
  def iterator: Iterator[Int] = throw new UnsupportedOperationException
  def keysIteratorFrom(start: Int): Iterator[Int] = throw new UnsupportedOperationException

  override def equals(o: Any) = o match {
    case ar: AnyRef => FreeInt eq ar
    case _ => false
  }
  override def toString = "?"
  def fullyDefined = false
}
