package cspom.util

import scala.collection.SortedSet
import com.google.common.collect.DiscreteDomain
import Interval.AsOrdered

class ContiguousRangeSet[A](val r: RangeSet[A], val d: DiscreteDomain[AsOrdered[A]])(
  implicit val ordering: Ordering[A]) extends SortedSet[A] {

  def iterator: Iterator[A] = r.ranges.iterator.flatMap(_.allValues(d))
  def -(elem: A): SortedSet[A] = new ContiguousRangeSet(r -- Interval.singleton(elem), d)
  def +(elem: A): SortedSet[A] = new ContiguousRangeSet(r ++ Interval.singleton(elem), d)
  def contains(elem: A): Boolean = r.contains(elem)

  def keysIteratorFrom(start: A): Iterator[A] =
    (r & Interval.atLeast(start)).ranges.iterator.flatMap(_.allValues(d))

  def rangeImpl(from: Option[A], until: Option[A]): SortedSet[A] = {
    val i = from.map(Interval.atLeast(_)).getOrElse(Interval.all[A]) &
      until.map(Interval.atMost(_)).getOrElse(Interval.all[A])

    new ContiguousRangeSet(r & i, d)
  }

  override def last = r.lastInterval.lastValue(d)

  override def size = r.ranges.map(_.nbValues(d)).sum

  def singletonMatch: Option[A] = {
    toStream match {
      case Stream(c) => Some(c.value)
      case _ => None
    }
  }
}