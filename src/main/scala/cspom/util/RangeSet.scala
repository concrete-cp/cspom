package cspom.util

import com.google.common.collect.DiscreteDomain
import cspom.util.GuavaRange.AsOrdered
import com.google.common.collect.ContiguousSet
import scala.collection.JavaConversions

object RangeSet {
  //  def apply[A <% Ordered[A]](s: Iterable[A]): RangeSet[A] = s match {
  //    case r: Range if r.step == 1 => RangeSet(
  //      GuavaRange.closed(r.head.asInstanceOf[A], r.last.asInstanceOf[A]))
  //    case i: RangeSet[A] => i
  //    case s => s.foldLeft(empty[A])(_ + GuavaRange.of(_))
  //  }

  def apply[A <% Ordered[A]](s: Iterable[GuavaRange[A]]): RangeSet[A] = {
    s.foldLeft(RangeSet[A]())(_ + _)
  }

  def apply[A <% Ordered[A]](s: GuavaRange[A]): RangeSet[A] = {
    RangeSet[A]() + s
  }

  def all[A <% Ordered[A]]: RangeSet[A] = apply(GuavaRange.all[A]())

  def apply[A <% Ordered[A]](): RangeSet[A] = new NoIntervals()

}

sealed trait RangeSet[A] {
  def lastInterval: GuavaRange[A]
  def headInterval: GuavaRange[A]

  implicit def ordering: Ordering[A]

  def +(i: GuavaRange[A]): RangeSet[A]

  def -(i: GuavaRange[A]): RangeSet[A]

  def ++(i: RangeSet[A]): RangeSet[A] = this ++ i.ranges

  def ++(i: Traversable[GuavaRange[A]]): RangeSet[A] = i.foldLeft(this)(_ + _)

  def ranges: Seq[GuavaRange[A]]

  def --(i: RangeSet[A]): RangeSet[A] = i.ranges.foldLeft(this)(_ - _)

  def &(i: RangeSet[A]): RangeSet[A] = this -- (this -- i)

  def &(i: GuavaRange[A]): RangeSet[A] = this -- (this - i)

  def removeLast: RangeSet[A]

  def upperBound: A = lastInterval.upperEndpoint

  def lowerBound: A = headInterval.lowerEndpoint

  def fullyDefined = lastInterval.hasLowerBound && lastInterval.hasUpperBound

  def isConvex: Boolean

  def isEmpty: Boolean

  def contains(elem: A): Boolean

  def canonical(implicit d: DiscreteDomain[AsOrdered[A]]) =
    RangeSet(ranges.map(_.canonical))

  def allValues(implicit d: DiscreteDomain[AsOrdered[A]]) =
    ranges.iterator.flatMap(_.allValues)

  def firstValue(implicit d: DiscreteDomain[AsOrdered[A]]) =
    headInterval.firstValue

  def lastValue(implicit d: DiscreteDomain[AsOrdered[A]]) =
    lastInterval.lastValue

  def singletonMatch(implicit d: DiscreteDomain[AsOrdered[A]]): Option[A] = {
    allValues(d).toStream match {
      case Stream(c) => Some(c.value)
      case _ => None
    }
  }

  override def equals(o: Any): Boolean = {
    o match {
      case i: RangeSet[_] => i.ranges == ranges
      case s => false
    }
  }

  override def toString = ranges.mkString("{", ", ", "}")
}

final case class NoIntervals[A](implicit val ordering: Ordering[A]) extends RangeSet[A] {

  def lastInterval = throw new UnsupportedOperationException
  def headInterval = throw new UnsupportedOperationException

  def +(i: GuavaRange[A]) = {
    if (i.isEmpty) this else new SomeIntervals[A](i, this, this)
  }

  def -(i: GuavaRange[A]) = this

  def removeLast = this

  def ranges = Seq()

  def isConvex = true

  def contains(elem: A) = false

  def isEmpty = true

}

final case class SomeIntervals[A](
  val range: GuavaRange[A],
  val lower: RangeSet[A],
  val upper: RangeSet[A])(implicit val ordering: Ordering[A]) extends RangeSet[A] {

  require(!range.isEmpty)

  implicit def asOrdered(v: A) = Ordered.orderingToOrdered(v)(ordering)

  // If children are defined, they must not be empty and be correctly ordered
  require(lower match {
    case _: NoIntervals[_] => true
    case SomeIntervals(r, _, _) => r isBefore range
  })

  require(upper match {
    case _: NoIntervals[_] => true
    case SomeIntervals(r, _, _) => r isAfter range
  })

  def isConvex = lower.isEmpty && upper.isEmpty

  def +(i: GuavaRange[A]): RangeSet[A] = {
    if (i.isEmpty) {
      this
    } else if (i.isConnected(range)) {
      val newItv = i.span(range) // union GuavaRange
      removeTop + newItv
    } else if (i.hasLowerBound && range.hasUpperBound && i.lowerEndpoint > range.upperEndpoint) {
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

  def -(i: GuavaRange[A]): RangeSet[A] = {

    val l: RangeSet[A] =
      if (!i.hasLowerBound || range.hasLowerBound && i.lowerEndpoint <= range.lowerEndpoint)
        lower - i
      else
        lower

    val u: RangeSet[A] =
      if (!i.hasUpperBound || range.hasUpperBound && i.upperEndpoint >= range.upperEndpoint)
        upper - i
      else
        upper

    val before = i.lowerBoundOption map {
      case (lep, lbt) => GuavaRange.upTo(lep, lbt.other)
    }
    val after = i.upperBoundOption map {
      case (uep, ubt) => GuavaRange.downTo(uep, ubt.other)
    }

    var newT = if (l.isEmpty) {
      u
    } else {
      new SomeIntervals(l.lastInterval, l.removeLast, u)
    }

    for (b <- before if b.isConnected(range)) {
      newT += b & range
    }
    for (a <- after if a.isConnected(range)) {
      newT += a & range
    }

    newT
  }

  def contains(elem: A): Boolean = {
    if (range.contains(elem)) {
      true
    } else if (elem < range.lowerEndpoint) {
      lower.contains(elem)
    } else if (elem > range.upperEndpoint) {
      upper.contains(elem)
    } else {
      false
    }
  }

  def isEmpty = false
}