package cspom.util

import scala.collection.immutable.TreeSet

import com.google.common.collect.DiscreteDomain

import cspom.util.Interval.AsOrdered

class IntervalOrdering[A <% Ordered[A]] extends Ordering[Interval[A]] {
  def compare(i: Interval[A], j: Interval[A]) = {
    if (i isConnected j) {
      0
    } else if (i isAfter j) {
      1
    } else {
      -1
    }
  }
}

object RangeSet {
  //  def apply[A <% Ordered[A]](s: Iterable[A]): RangeSet[A] = s match {
  //    case r: Range if r.step == 1 => RangeSet(
  //      Interval.closed(r.head.asInstanceOf[A], r.last.asInstanceOf[A]))
  //    case i: RangeSet[A] => i
  //    case s => s.foldLeft(empty[A])(_ + Interval.of(_))
  //  }

  def apply[A <% Ordered[A]](s: Iterable[Interval[A]]): RangeSet[A] = {
    s.foldLeft(RangeSet[A]())(_ ++ _)
  }

  def apply[A <% Ordered[A]](s: Interval[A]): RangeSet[A] = {
    RangeSet[A]() ++ s
  }

  def all[A <% Ordered[A]]: RangeSet[A] = apply(Interval.all[A]())

  def apply[A <% Ordered[A]](): RangeSet[A] = new RangeSet(TreeSet()(new IntervalOrdering))

  implicit def valueAsSingletonRange[A <% Ordered[A]](i: A) = Interval.singleton(i)

  implicit def rangeAsRangeSet[A <% Ordered[A]](i: Interval[A]) = RangeSet(i)

  implicit def valueasRangeSet[A <% Ordered[A]](i: A) = RangeSet(i)
}

final class RangeSet[A](contents: TreeSet[Interval[A]])(implicit val ordering: Ordering[A]) {
  def lastInterval: Interval[A] = contents.last
  def headInterval: Interval[A] = contents.head

  //implicit def ordering: Ordering[A]

  def ++(i: Interval[A]): RangeSet[A] = {
    if (i.isEmpty) {
      this
    } else {
      val itvOrdering = contents.ordering

      val colliding = contents.from(i).takeWhile(j => itvOrdering.compare(i, j) == 0)

      new RangeSet(
        (contents -- colliding) + colliding.fold(i)(_ span _))
    }
  }

  def --(i: Interval[A]): RangeSet[A] = {
    val itvOrdering = contents.ordering

    val (colliding, afterTree) = contents.from(i).span(j => itvOrdering.compare(i, j) == 0)

    val beforeTree = contents.until(i)

    val beforeItv = i.lowerBoundOption map {
      case (lep, lbt) => Interval.upTo(lep, lbt.other)
    }
    val afterItv = i.upperBoundOption map {
      case (uep, ubt) => Interval.downTo(uep, ubt.other)
    }

    var cleanTree = beforeTree ++ afterTree

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

    new RangeSet(cleanTree)
  }

  def ++(i: RangeSet[A]): RangeSet[A] = this ++ i.ranges

  def ++(i: Traversable[Interval[A]]): RangeSet[A] = i.foldLeft(this)(_ ++ _)

  def ranges: Iterable[Interval[A]] = contents

  def --(i: RangeSet[A]): RangeSet[A] = i.ranges.foldLeft(this)(_ -- _)

  def &(i: RangeSet[A]): RangeSet[A] = this -- (this -- i)

  def &(i: Interval[A]): RangeSet[A] = this -- (this -- i)

  def upperBound: A = lastInterval.upperEndpoint

  def lowerBound: A = headInterval.lowerEndpoint

  def fullyDefined = lastInterval.hasLowerBound && lastInterval.hasUpperBound

  def isConvex: Boolean = contents.size == 1

  def isEmpty: Boolean = contents.isEmpty

  def contains(elem: A): Boolean = contents.from(Interval.singleton(elem)).head.contains(elem)

  def canonical(implicit d: DiscreteDomain[AsOrdered[A]]) =
    RangeSet[A]() ++ contents.map(_.canonical)

  override def equals(o: Any): Boolean = {
    o match {
      case i: RangeSet[_] => i.ranges.iterator.sameElements(ranges.iterator)
      case s => false
    }
  }

  override def toString = ranges.mkString("{", ", ", "}")
}
