package cspom.util

import scala.collection.immutable.SortedSet
import scala.collection.immutable.TreeSet

class IntIntervalOrdering[@specialized T] extends Ordering[Interval[T]] {
  def compare(i: Interval[T], j: Interval[T]) = {
    if (i isAfter j) {
      1
    } else if (i isBefore j) {
      -1
    } else {
      0
    }
  }
}

object RangeSet {
  //  def apply(s: Iterable[A]): RangeSet = s match {
  //    case r: Range if r.step == 1 => RangeSet(
  //      Interval.closed(r.head.asInstanceOf[A], r.last.asInstanceOf[A]))
  //    case i: RangeSet => i
  //    case s => s.foldLeft(empty[A])(_ + Interval.of(_))
  //  }

  def apply[T](s: Iterable[Interval[T]]): RangeSet[T] = {
    s.foldLeft(RangeSet[T]())(_ ++ _)
  }

  def apply[T](s: Interval[T]): RangeSet[T] = {
    RangeSet[T]() ++ s
  }

  val allInt: RangeSet[Infinitable] = apply(IntInterval.all)

  def apply[T](): RangeSet[T] = new RangeSet(TreeSet()(new IntIntervalOrdering))

}

final class RangeSet[@specialized T](private val contents: TreeSet[Interval[T]]) {
  def lastInterval: Interval[T] = contents.last
  def headInterval: Interval[T] = contents.head

  def span = headInterval span lastInterval

  def ++(i: Interval[T]): RangeSet[T] = {
    if (i.isEmpty) {
      this
    } else {
      val colliding = contents.keysIteratorFrom(i).takeWhile(i.isConnected).toSeq

      val collSpan = if (colliding.isEmpty) {
        i
      } else {
        i.span(colliding.head.lb, colliding.last.ub)
      }

      new RangeSet(
        (contents -- colliding) + collSpan)
    }
  }

  def --(i: Interval[T]): RangeSet[T] = {
    val itvOrdering = contents.ordering

    val colliding = contents.from(i).takeWhile(i.isConnected)

    var cleanTree = contents -- colliding

    for (c <- colliding) {

      val b = c.lessThan(i.lb)
      if (!b.isEmpty) {
        cleanTree += b
      }
      val a = c.moreThan(i.ub)
      if (!a.isEmpty) {
        cleanTree += a
      }
    }

    new RangeSet(cleanTree)
  }

  def ++(i: RangeSet[T]): RangeSet[T] = this ++ i.ranges

  def ++(i: Iterable[Interval[T]]): RangeSet[T] = i.foldLeft(this)(_ ++ _)

  def ranges: SortedSet[Interval[T]] = contents

  def --(i: RangeSet[T]): RangeSet[T] = i.ranges.foldLeft(this)(_ -- _)

  def &(i: RangeSet[T]): RangeSet[T] = this -- (this -- i)

  def &(i: Interval[T]): RangeSet[T] = this -- (this -- i)

  def upperBound: T = lastInterval.ub

  def lowerBound: T = headInterval.lb

  def fullyDefined = {
    lowerBound != MinInf && upperBound != PlusInf
  }

  def isConvex: Boolean = contents.size == 1

  def isEmpty: Boolean = contents.isEmpty

  def intersects(elem: Interval[T]): Boolean = {
    val interval = contents.keysIteratorFrom(elem)
    interval.hasNext && interval.next().intersects(elem)
    //    } else {
    //      
    //    }
    //    contents.exists(_.contains(elem))
  }

  override def equals(o: Any): Boolean = {
    // println(this.ranges == o.asInstanceOf[RangeSet[_]].ranges)
    o match {
      case i: RangeSet[_] => i.ranges.sameElements(ranges)
      case s              => false
    }
  }

  override def toString = ranges.mkString("{", ", ", "}")

}
