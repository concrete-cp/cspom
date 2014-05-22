package cspom.util

import scala.collection.immutable.SortedSet
import scala.collection.GenTraversableOnce
import com.google.common.collect.BoundType
import AsOrdered.asOrdered

object RangeSet {
  def apply[A <% Ordered[A]](s: Iterable[A]): RangeSet[A] = s match {
    case r: Range if r.step == 1 => RangeSet(r.head.asInstanceOf[A], r.last.asInstanceOf[A])
    case i: RangeSet[A] => i
    case s => s.foldLeft(empty[A])(_ + rangeValue(_))
  }

  def apply[A <% Ordered[A]](itv: GuavaRange[AsOrdered[A]]): RangeSet[A] = empty[A] + itv

  def apply[A <% Ordered[A]](lb: A, ub: A): RangeSet[A] =
    RangeSet(GuavaRange.closed(lb, ub))

  def apply[A <% Ordered[A]]() = empty[A]

  def empty[A <% Ordered[A]]: RangeSet[A] = NoIntervals.asInstanceOf[RangeSet[A]]

  def rangeValue[A <% Ordered[A]](v: A) = GuavaRange.closed(v, v)
}

sealed trait RangeSet[A <: Ordered[A]] {
  def lastInterval: GuavaRange[A]
  def headInterval: GuavaRange[A]

  def +(i: GuavaRange[A]): RangeSet[A]

  def -(i: GuavaRange[A]): RangeSet[A]

  def ++(i: RangeSet[A]): RangeSet[A] = this ++ i.ranges

  def ++(i: Traversable[GuavaRange[A]]): RangeSet[A] = i.foldLeft(this)(_ + _)

  def ranges: Seq[GuavaRange[A]]

  def --(i: RangeSet[A]): RangeSet[A] = i.ranges.foldLeft(this)(_ - _)

  def &(i: RangeSet[A]): RangeSet[A] = this -- (this -- i)

  def &(i: GuavaRange[A]): RangeSet[A] = this -- (this - i)

  def removeLast: RangeSet[A]

  def upperBound: A = lastInterval.upperEndpoint()

  def lowerBound: A = headInterval.lowerEndpoint()

  def isConvex: Boolean

  def isEmpty: Boolean = this eq NoIntervals

  def contains(elem: A): Boolean
}

object NoIntervals extends RangeSet[Nothing] {
  def lastInterval = throw new UnsupportedOperationException
  def headInterval = throw new UnsupportedOperationException

  def +(i: GuavaRange[Nothing]) = {
    if (i.isEmpty) this else new SomeIntervals[Nothing](i, NoIntervals, NoIntervals)
  }

  def -(i: GuavaRange[Nothing]) = this

  def removeLast = this

  def ranges = Seq()

  override def toString = "[]"

  def isConvex = true

  def contains(elem: Nothing) = false
}

final class SomeIntervals[A <% Ordered[A]](
  val range: GuavaRange[A],
  val lower: RangeSet[A],
  val upper: RangeSet[A]) extends RangeSet[A] {

  require(!range.isEmpty)

  implicit class BARange(g: GuavaRange[A]) {
    def isBefore(h: GuavaRange[A]) = {
      !g.isConnected(h) && g.upperEndpoint() < h.lowerEndpoint()
    }
    def isAfter(h: GuavaRange[A]) = {
      !g.isConnected(h) && h.upperEndpoint() < g.lowerEndpoint()
    }
  }

  // If children are defined, they must not be empty and be correctly ordered
  require(lower.isEmpty || (lower.lastInterval isBefore range))
  require(upper.isEmpty || (upper.headInterval isAfter range))

  def isConvex = lower.isEmpty && upper.isEmpty

  def +(i: GuavaRange[A]): RangeSet[A] = {
    if (i.isConnected(range)) {
      val newItv = i.span(range) // union GuavaRange
      removeTop + newItv
    } else if (i.lowerEndpoint() > range.upperEndpoint()) {
      new SomeIntervals(range, lower, upper + i)
    } else {
      new SomeIntervals(range, lower + i, upper)
    }
  }

  def removeLast: RangeSet[A] = {
    if (upper.isEmpty) {
      lower
    } else {
      new SomeIntervals(range, lower, upper.removeLast)
    }
  }

  private def removeTop: RangeSet[A] = {
    if (lower.isEmpty) {
      upper
    } else {
      new SomeIntervals(lower.lastInterval, lower.removeLast, upper)
    }
  }

  def convex = lower.isEmpty && upper.isEmpty

  def lastInterval: GuavaRange[A] = {
    if (upper.isEmpty) {
      range
    } else {
      upper.lastInterval
    }
  }

  def headInterval: GuavaRange[A] = {
    if (lower.isEmpty) {
      range
    } else {
      lower.headInterval
    }
  }

  def ranges: Seq[GuavaRange[A]] = {
    lower.ranges ++: range +: upper.ranges
  }

  override def equals(o: Any): Boolean = {
    o match {
      case i: RangeSet[_] => i.ranges == ranges
      case s => super.equals(s)
    }
  }

  def other(bt: BoundType) = bt match {
    case BoundType.CLOSED => BoundType.OPEN
    case BoundType.OPEN => BoundType.CLOSED
  }

  def -(i: GuavaRange[A]): RangeSet[A] = {
    val l = if (i.lowerEndpoint() <= range.lowerEndpoint()) lower - i else lower
    val u = if (i.upperEndpoint() >= range.upperEndpoint()) upper - i else upper

    val before = GuavaRange.upTo(i.lowerEndpoint(), other(i.lowerBoundType()))
    val after = GuavaRange.downTo(i.upperEndpoint(), other(i.lowerBoundType()))

    var newT = if (l.isEmpty) {
      u
    } else {
      new SomeIntervals(l.lastInterval, l.removeLast, u)
    }

    if (before.isConnected(range)) {
      newT += before.intersection(range)
    }
    if (after.isConnected(range)) {
      newT += after.intersection(range)
    }

    newT
  }

  def contains(elem: A): Boolean = {
    if (range.contains(elem)) {
      true
    } else if (elem < range.lowerEndpoint()) {
      lower.contains(elem)
    } else if (elem > range.upperEndpoint()) {
      upper.contains(elem)
    } else {
      false
    }
  }

  override def toString = ranges.mkString(" + ")

}