package cspom.variable

import scala.collection.immutable.SortedSet
import scala.collection.GenTraversableOnce

object Intervals {
  def apply(s: Iterable[Int]): Intervals = s match {
    case r: Range if r.step == 1 => Intervals(r.head, r.last)
    case i: Intervals => i
    case s => s.foldLeft(empty)(_ + _)
  }

  def apply(itv: Interval): Intervals = NoIntervals + itv

  def apply(lb: Int, ub: Int): Intervals = Intervals(Interval(lb, ub))

  def apply() = empty

  val empty: Intervals = NoIntervals
}

sealed trait Intervals extends SortedSet[Int] {
  def lastInterval: Interval
  def headInterval: Interval

  def +(i: Interval): Intervals

  def -(i: Interval): Intervals

  def ++(i: Intervals): Intervals = this ++ i.intervals

  def ++(i: Traversable[Interval]): Intervals = i.foldLeft(this)(_ + _)

  override def ++(i: GenTraversableOnce[Int]): Intervals = i.foldLeft(this)(_ + _)

  def intervals: Seq[Interval]

  def --(i: Intervals): Intervals = i.intervals.foldLeft(this)(_ - _)

  def &(i: Intervals): Intervals = this -- (this -- i)

  def &(i: Interval): Intervals = this -- (this - i)

  def removeLast: Intervals

  override def last: Int = lastInterval.ub

  override def head: Int = headInterval.lb

  def iterator = intervals.iterator.flatMap(_.range.iterator)

  // Members declared in  scala.collection.generic.Sorted 
  def keysIteratorFrom(start: Int): Iterator[Int] = ???
  // Members declared in scala.collection.SortedSetLike 
  implicit def ordering: Ordering[Int] = Ordering.Int

  def rangeImpl(from: Option[Int], until: Option[Int]): SortedSet[Int] = ???

  // Members declared in scala.collection.SetLike 
  def -(elem: Int): Intervals = this - Interval(elem, elem)

  def +(elem: Int): Intervals = this + Interval(elem, elem)

  def isConvex: Boolean
}

object NoIntervals extends Intervals {
  def lastInterval = throw new UnsupportedOperationException
  def headInterval = throw new UnsupportedOperationException

  def +(i: Interval) = {
    if (i.isEmpty) this else new SomeIntervals(i, NoIntervals, NoIntervals)
  }

  def -(i: Interval) = this

  def removeLast = this

  override def size = 0

  def intervals = Seq()

  def contains(elem: Int) = false

  override def toString = "[]"

  def isConvex = true
}

final class SomeIntervals(
  val interval: Interval,
  val lower: Intervals,
  val upper: Intervals) extends Intervals {

  require(interval.nonEmpty)

  // If children are defined, they must not be empty and be correctly ordered
  require(lower.isEmpty || (lower.lastInterval isBefore interval))
  require(upper.isEmpty || (upper.headInterval isAfter interval))
  
  def isConvex = lower.isEmpty && upper.isEmpty

  def +(i: Interval): Intervals = {
    if (i isBefore interval) {
      new SomeIntervals(interval, lower + i, upper)
    } else if (i isAfter interval) {
      new SomeIntervals(interval, lower, upper + i)
    } else {
      val newItv = i union interval
      removeTop + newItv
    }

  }

  def removeLast: Intervals = {
    if (upper.isEmpty) {
      lower
    } else {
      new SomeIntervals(interval, lower, upper.removeLast)
    }
  }

  private def removeTop: Intervals = {
    if (lower.isEmpty) {
      upper
    } else {
      new SomeIntervals(lower.lastInterval, lower.removeLast, upper)
    }
  }

  override val size: Int = {
    val l = interval.size.toLong + lower.size + upper.size
    require(l <= Int.MaxValue)
    l.toInt
  }

  def convex = lower.isEmpty && upper.isEmpty

  def lastInterval: Interval = {
    if (upper.isEmpty) {
      interval
    } else {
      upper.lastInterval
    }
  }

  def headInterval: Interval = {
    if (lower.isEmpty) {
      interval
    } else {
      lower.headInterval
    }
  }

  def intervals: Seq[Interval] = {
    lower.intervals ++: interval +: upper.intervals
  }

  override def equals(o: Any): Boolean = {
    o match {
      case i: Intervals => i.intervals == intervals
      case s => super.equals(s)
    }
  }

  def -(i: Interval): Intervals = {
    val l = if (i.lb < interval.lb) lower - i else lower
    val u = if (i.ub > interval.ub) upper - i else upper

    val newI = interval diff i

    val newT = if (l.isEmpty) {
      u
    } else {
      new SomeIntervals(l.lastInterval, l.removeLast, u)
    }

    newI.foldLeft(newT)(_ + _)
  }

  def contains(elem: Int): Boolean = {
    if (interval.contains(elem)) {
      true
    } else if (elem < interval.lb) {
      lower.contains(elem)
    } else if (elem > interval.ub) {
      upper.contains(elem)
    } else {
      false
    }
  }

  override def toString = intervals.mkString(" + ")

}