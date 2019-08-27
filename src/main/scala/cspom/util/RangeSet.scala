package cspom.util

import scala.collection.immutable.TreeSet

object RangeSet {

  val allInt: RangeSet[Infinitable] = apply(IntInterval.all)

  def apply[T](s: Iterable[Interval[T]]): RangeSet[T] = {
    s.foldLeft(empty[T])(_ ++ _)
  }

  def empty[T]: RangeSet[T] = new RangeSet(itvTreeSet())

  def itvTreeSet[T](itv: Interval[T]*): TreeSet[Interval[T]] =
    TreeSet[Interval[T]](itv: _*)(new IntervalOrdering)

  def apply[T](s: Interval[T]): RangeSet[T] = {
    if (s.isEmpty) empty else
      new RangeSet(itvTreeSet(s))
  }

}

final class RangeSet[@specialized T](val contents: TreeSet[Interval[T]]) {
  def span: Interval[T] = headInterval span lastInterval

  def lastInterval: Interval[T] = contents.last

  def headInterval: Interval[T] = contents.head

  def ++(i: RangeSet[T]): RangeSet[T] = this ++ i.contents

  def ++(i: Iterable[Interval[T]]): RangeSet[T] = i.foldLeft(this)(_ ++ _)

  def ++(i: Interval[T]): RangeSet[T] = {
    if (i.isEmpty) {
      this
    } else {
      val colliding = contents.iteratorFrom(i).takeWhile(i.isConnected).toSeq

      val collSpan = if (colliding.isEmpty) {
        i
      } else {
        i.span(colliding.head.lb, colliding.last.ub)
      }

      new RangeSet(
        (contents -- colliding) + collSpan)
    }
  }

  def &(i: RangeSet[T]): RangeSet[T] = {
    this -- (this -- i)
  }

  def --(i: RangeSet[T]): RangeSet[T] = i.contents.foldLeft(this)(_ -- _)

  def --(i: Interval[T]): RangeSet[T] = {
    if (i.isEmpty) this
    else {
      val colliding = contents.rangeFrom(i).takeWhile(i.isConnected)

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
  }

  def &(i: Interval[T]): RangeSet[T] = this -- (this -- i)

  def fullyDefined: Boolean = {
    lowerBound != MinInf && upperBound != PlusInf
  }

  def upperBound: T = lastInterval.ub

  def lowerBound: T = headInterval.lb

  def isConvex: Boolean = contents.size == 1

  def intersects(elem: Interval[T]): Boolean = {
    val interval = contents.iteratorFrom(elem)
    interval.hasNext && interval.next().intersects(elem)
    //    } else {
    //
    //    }
    //    contents.exists(_.contains(elem))
  }

  override def equals(o: Any): Boolean = {
    // println(this.ranges == o.asInstanceOf[RangeSet[_]].ranges)
    o match {
      case i: RangeSet[_] => i.contents.iterator.sameElements(contents)
      case s => false
    }
  }

  override def toString: String = {
    if (isEmpty) "∅" else contents.mkString("∪")
  }

  def isEmpty: Boolean = contents.isEmpty

}
