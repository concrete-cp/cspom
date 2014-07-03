package cspom.util

import scala.collection.immutable.TreeSet
import com.google.common.collect.DiscreteDomain
import scala.collection.immutable.SortedSet

object IntIntervalOrdering extends Ordering[IntInterval] {
  def compare(i: IntInterval, j: IntInterval) = {
    if (i isAfter j) {
      1
    } else if (i isBefore j) {
      -1
    } else {
      0
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

  val all: IntRangeSet = apply(IntInterval.all)

  def apply(): IntRangeSet = new IntRangeSet(TreeSet()(IntIntervalOrdering))

  implicit def valueAsSingletonRange(i: Int) = IntInterval.singleton(i)

  implicit def rangeAsRangeSet(i: IntInterval) = IntRangeSet(i)

  implicit def valueasRangeSet(i: Int) = IntRangeSet(i)
}

final class IntRangeSet(private val contents: TreeSet[IntInterval]) {
  def lastInterval: IntInterval = contents.last
  def headInterval: IntInterval = contents.head

  //implicit def ordering: Ordering[A]

  def span = headInterval span lastInterval

  def ++(i: IntInterval): IntRangeSet = {
    if (i.isEmpty) {
      this
    } else {
      val colliding = contents.from(i).takeWhile(i.isConnected)

      new IntRangeSet(
        (contents -- colliding) + colliding.fold(i)(_ span _))
    }
  }

  //  def ++(i: IntRangeSet): IntRangeSet = {
  //    import IntIntervalOrdering.compare
  //    var it1 = contents.toStream
  //    var it2 = i.contents.toStream
  //    var result = TreeSet.newBuilder(IntIntervalOrdering)
  //    result.sizeHint(contents.size + i.contents.size)
  //
  //    while (it1.nonEmpty && it2.nonEmpty) {
  //      val comp = compare(it1.head, it2.head)
  //      if (comp == 0) {
  //        it1 = (it1.head span it2.head) +: it1.tail
  //        it2 = it2.tail
  //      } else if (comp < 0) {
  //        result += it1.head
  //        it1 = it1.tail
  //      } else {
  //        result += it2.head
  //        it2 = it2.tail
  //      }
  //    }
  //
  //    result ++= it1
  //    result ++= it2
  //
  //    IntRangeSet(result.result)
  //  }

  def --(i: IntInterval): IntRangeSet = {
    val itvOrdering = contents.ordering

    val colliding = contents.from(i).takeWhile(i.isConnected)

    val beforeItv = i.lb match {
      case MinInf => None
      case Finite(l) => if (l == Int.MinValue) None else Some(IntInterval.atMost(l - 1))
      case PlusInf => throw new AssertionError
    }

    val afterItv = i.ub match {
      case PlusInf => None
      case Finite(u) => if (u == Int.MaxValue) None else Some(IntInterval.atLeast(u + 1))
      case MinInf => throw new AssertionError
    }

    var cleanTree: TreeSet[IntInterval] = contents -- colliding

    for (b <- beforeItv; c <- colliding) {
      val d = b & c
      if (!d.isEmpty) {
        cleanTree += d
      }
    }

    for (a <- afterItv; c <- colliding) {
      val d = a & c
      if (!d.isEmpty) {
        cleanTree += d
      }
    }

    new IntRangeSet(cleanTree)
  }

  def ++(i: IntRangeSet): IntRangeSet = this ++ i.ranges

  def ++(i: Iterable[IntInterval]): IntRangeSet = i.foldLeft(this)(_ ++ _)

  def ranges: SortedSet[IntInterval] = contents

  def --(i: IntRangeSet): IntRangeSet = i.ranges.foldLeft(this)(_ -- _)

  def &(i: IntRangeSet): IntRangeSet = this -- (this -- i)

  def &(i: IntInterval): IntRangeSet = this -- (this -- i)

  def upperBound: Infinitable = lastInterval.ub

  def lowerBound: Infinitable = headInterval.lb

  def fullyDefined = {
    lowerBound != MinInf && upperBound != PlusInf
  }

  def isConvex: Boolean = contents.size == 1

  def isEmpty: Boolean = contents.isEmpty

  def contains(elem: Int): Boolean =
    contents.from(IntInterval.singleton(elem)).headOption.exists(_.contains(elem))

  override def equals(o: Any): Boolean = {
    o match {
      case i: IntRangeSet => i.ranges.iterator.sameElements(ranges.iterator)
      case s => false
    }
  }

  override def toString = ranges.mkString("{", ", ", "}")
}
