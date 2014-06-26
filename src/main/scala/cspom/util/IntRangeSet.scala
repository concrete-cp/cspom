package cspom.util

import scala.collection.immutable.TreeSet
import com.google.common.collect.DiscreteDomain
import scala.collection.immutable.SortedSet

object IntIntervalOrdering extends Ordering[IntInterval] {
  def compare(i: IntInterval, j: IntInterval) = {
    if (i isConnected j) {
      0
    } else if (i isAfter j) {
      1
    } else {
      -1
    }
  }
}

object IntRangeSet {
  //  def apply(s: Iterable[A]): IntRangeSet = s match {
  //    case r: Range if r.step == 1 => RangeSet(
  //      Interval.closed(r.head.asInstanceOf[A], r.last.asInstanceOf[A]))
  //    case i: IntRangeSet => i
  //    case s => s.foldLeft(empty[A])(_ + Interval.of(_))
  //  }

  def apply(s: Iterable[IntInterval]): IntRangeSet = {
    s.foldLeft(IntRangeSet())(_ ++ _)
  }

  def apply(s: IntInterval): IntRangeSet = {
    IntRangeSet() ++ s
  }

  def all: IntRangeSet = apply(IntInterval.all())

  def apply(): IntRangeSet = new IntRangeSet(TreeSet()(IntIntervalOrdering))

  implicit def valueAsSingletonRange(i: Int) = IntInterval.singleton(i)

  implicit def rangeAsRangeSet(i: IntInterval) = IntRangeSet(i)

  implicit def valueasRangeSet(i: Int) = IntRangeSet(i)
}

final class IntRangeSet(contents: TreeSet[IntInterval]) {
  def lastInterval: IntInterval = contents.last
  def headInterval: IntInterval = contents.head

  //implicit def ordering: Ordering[A]

  def span = headInterval span lastInterval

  def ++(i: IntInterval): IntRangeSet = {
    if (i.isEmpty) {
      this
    } else {
      val itvOrdering = contents.ordering

      val colliding = contents.from(i).takeWhile(j => itvOrdering.compare(i, j) == 0)

      new IntRangeSet(
        (contents -- colliding) + colliding.fold(i)(_ span _))
    }
  }

  def --(i: IntInterval): IntRangeSet = {
    val itvOrdering = contents.ordering

    val colliding = contents.from(i).takeWhile(j => itvOrdering.compare(i, j) == 0)

    val beforeItv = i.lowerBoundOption map {
      case (lep, lbt) => IntInterval.upTo(lep, lbt.other)
    }
    val afterItv = i.upperBoundOption map {
      case (uep, ubt) => IntInterval.downTo(uep, ubt.other)
    }

    var cleanTree: TreeSet[IntInterval] = contents -- colliding

    for (b <- beforeItv; c <- colliding if (b isConnected c)) {
      val d = b & c
      if (!d.isEmpty) {
        cleanTree += d
      }
    }

    for (a <- afterItv; c <- colliding if (a isConnected c)) {
      val d = a & c
      if (!d.isEmpty) {
        cleanTree += d
      }
    }

    new IntRangeSet(cleanTree)
  }

  def ++(i: IntRangeSet): IntRangeSet = this ++ i.ranges

  def ++(i: Traversable[IntInterval]): IntRangeSet = i.foldLeft(this)(_ ++ _)

  def ranges: Iterable[IntInterval] = contents

  def --(i: IntRangeSet): IntRangeSet = i.ranges.foldLeft(this)(_ -- _)

  def &(i: IntRangeSet): IntRangeSet = this -- (this -- i)

  def &(i: IntInterval): IntRangeSet = this -- (this -- i)

  def upperBound: Integer = lastInterval.upperEndpoint

  def lowerBound: Integer = headInterval.lowerEndpoint

  def fullyDefined = lastInterval.hasLowerBound && lastInterval.hasUpperBound

  def isConvex: Boolean = contents.size == 1

  def isEmpty: Boolean = contents.isEmpty

  def contains(elem: Integer): Boolean =
    contents.from(IntInterval.singleton(elem)).headOption.exists(_.contains(elem))

  def canonical =
    IntRangeSet() ++ contents.map(_.canonical)

  override def equals(o: Any): Boolean = {
    o match {
      case i: IntRangeSet => i.ranges.iterator.sameElements(ranges.iterator)
      case s => false
    }
  }

  override def toString = ranges.mkString("{", ", ", "}")
}
