package cspom.util

import scala.collection.immutable.TreeSet

object RangeSet {

  def apply[T](s: Iterable[Interval[T]]): RangeSet[T] = {
    s.foldLeft(empty[T])(_ ++ _)
  }

  def apply[T](s: Interval[T]): RangeSet[T] = {
    if (s.isEmpty) empty else
      new RangeSet(itvTreeSet(s))

  }

  val allInt: RangeSet[Infinitable] = apply(IntInterval.all)

  def empty[T]: RangeSet[T] = new RangeSet(itvTreeSet())

  def itvTreeSet[T](itv: Interval[T]*): TreeSet[Interval[T]] =
    TreeSet[Interval[T]](itv: _*)(new IntervalOrdering)

}

final class RangeSet[@specialized T](val contents: TreeSet[Interval[T]]) {
  def lastInterval: Interval[T] = contents.last
  def headInterval: Interval[T] = contents.head

  def span: Interval[T] = headInterval span lastInterval

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

  def ++(i: RangeSet[T]): RangeSet[T] = this ++ i.contents

  def ++(i: Iterable[Interval[T]]): RangeSet[T] = i.foldLeft(this)(_ ++ _)

  def --(i: RangeSet[T]): RangeSet[T] = i.contents.foldLeft(this)(_ -- _)

  def &(i: RangeSet[T]): RangeSet[T] = this -- (this -- i)

  def &(i: Interval[T]): RangeSet[T] = this -- (this -- i)

  def upperBound: T = lastInterval.ub

  def lowerBound: T = headInterval.lb

  def fullyDefined: Boolean = {
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
      case i: RangeSet[_] => i.contents.sameElements(contents)
      case s              => false
    }
  }

  override def toString: String = {
    if (contents.size == 1) contents.head.toString
    else contents.mkString("{", ", ", "}")
  }

}
