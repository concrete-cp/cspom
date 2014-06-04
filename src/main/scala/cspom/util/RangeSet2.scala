package cspom.util

import scala.collection.immutable.TreeSet
import com.google.common.collect.DiscreteDomain
import cspom.util.Interval.AsOrdered

class IntervalOrdering[A <% Ordered[A]] extends Ordering[Interval[A]] {
  def compare(i: Interval[A], j: Interval[A]) = {
    if (i isAfter j) { 1 } else if (i isBefore j) { -1 } else { 0 }
  }
}

class RangeSet2[A](contents: TreeSet[Interval[A]]) {
  def lastInterval: Interval[A]
  def headInterval: Interval[A]

  implicit def ordering: Ordering[A]

  def ++(i: Interval[A]): RangeSet2[A] = {
    val after = contents.from(i)
    new RangeSet2(contents + i)
  }

  def --(i: Interval[A]): RangeSet2[A]

  def ++(i: RangeSet2[A]): RangeSet2[A] = this ++ i.ranges

  def ++(i: Traversable[Interval[A]]): RangeSet2[A] = i.foldLeft(this)(_ ++ _)

  def ranges: Seq[Interval[A]]

  def --(i: RangeSet2[A]): RangeSet2[A] = i.ranges.foldLeft(this)(_ -- _)

  def &(i: RangeSet2[A]): RangeSet2[A] = this -- (this -- i)

  def &(i: Interval[A]): RangeSet2[A] = this -- (this -- i)

  def removeLast: RangeSet2[A]

  def upperBound: A = lastInterval.upperEndpoint

  def lowerBound: A = headInterval.lowerEndpoint

  def fullyDefined = lastInterval.hasLowerBound && lastInterval.hasUpperBound

  def isConvex: Boolean

  def isEmpty: Boolean

  def contains(elem: A): Boolean

  def canonical(implicit d: DiscreteDomain[AsOrdered[A]]) =
    RangeSet(ranges.map(_.canonical))

  override def equals(o: Any): Boolean = {
    o match {
      case i: RangeSet2[_] => i.ranges == ranges
      case s => false
    }
  }

  override def toString = ranges.mkString("{", ", ", "}")
}
