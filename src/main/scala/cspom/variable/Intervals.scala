package cspom.variable

import scala.collection.immutable.SortedSet

object Intervals {
  def apply(s: Iterable[Int]): Intervals = s match {
    case r: Range if r.step == 1 => Intervals(r.head, r.last)
    case i: Intervals => i
    case s => s.foldLeft(empty)(_ + _)
  }

  def apply(itv: Interval): Intervals = new Intervals(itv, None, None)

  def apply(lb: Int, ub: Int): Intervals = Intervals(Interval(lb, ub))

  def apply() = empty

  val empty: Intervals = Intervals(0, -1)
}

final class Intervals private (
  val interval: Interval,
  val lower: Option[Intervals],
  val upper: Option[Intervals]) extends SortedSet[Int] {

  require((lower.isEmpty && upper.isEmpty) || interval.nonEmpty)

  // If children are defined, they must not be empty and be correctly ordered
  require(lower.forall { i => i.interval.nonEmpty && (i.interval isBefore interval) })
  require(upper.forall { i => i.interval.nonEmpty && (i.interval isAfter interval) })

  def +(i: Interval): Intervals = {
    if (interval.isEmpty) {
      Intervals(i)
    } else {
      add(i, Some(this))
    }
  }

  def ++(i: Intervals): Intervals = i.asSequence.foldLeft(this)(_ + _)

  private def add(i: Interval, itv: Option[Intervals]): Intervals = itv match {
    case None => Intervals(i)
    case Some(itv) =>
      val cItv = itv.interval
      assert(cItv.nonEmpty)
      if (i isBefore cItv) {
        new Intervals(cItv, Some(add(i, itv.lower)), itv.upper)
      } else if (i isAfter cItv) {
        new Intervals(cItv, itv.lower, Some(add(i, itv.upper)))
      } else {
        val newItv = i union cItv
        new Intervals(newItv, meldLower(newItv.lb, itv.lower), meldUpper(newItv.ub, itv.upper))
      }

  }

  override val size: Int = {
    val l = interval.size.toLong + lower.map(_.size).getOrElse(0) + upper.map(_.size).getOrElse(0)
    require(l <= Int.MaxValue)
    l.toInt
  }

  def convex = lower.isEmpty && upper.isEmpty

  @annotation.tailrec
  def max: Int = upper match {
    case None => interval.ub
    case Some(i) => i.max
  }

  @annotation.tailrec
  def min: Int = lower match {
    case None => interval.lb
    case Some(i) => i.min
  }

  // Should return the sets of intervals strictly before lb
  @annotation.tailrec
  def meldLower(lb: Int, itv: Option[Intervals]): Option[Intervals] = itv match {
    case None => None
    case Some(itv) =>
      if (lb - itv.max > 1) {
        Some(itv)
      } else if (lb < itv.interval.lb - 1) {
        meldLower(lb, itv.lower)
      } else {
        itv.lower
      }
  }

  // Should return the set of intervals strictly after ub
  @annotation.tailrec
  def meldUpper(ub: Int, itv: Option[Intervals]): Option[Intervals] = itv match {
    case None => None
    case Some(itv) =>
      if (itv.min - ub > 1) {
        Some(itv)
      } else if (ub > itv.interval.ub + 1) {
        meldUpper(ub, itv.upper)
      } else {
        itv.upper
      }
  }

  def asSequence: Seq[Interval] = {
    lower.toSeq.flatMap(_.asSequence) ++: interval +: upper.toSeq.flatMap(_.asSequence)
  }

  override def equals(o: Any): Boolean = {
    o match {
      case i: Intervals => i.asSequence == asSequence
      case s => super.equals(s)
    }
  }

  def -(i: Interval): Intervals = ???

  def --(i: Intervals): Intervals = i.asSequence.foldLeft(this)(_ - _)

  def intersect(i: Intervals): Intervals = this -- (this -- i)

  def intersect(i: Interval): Intervals = this -- (this - i)

  // Members declared in scala.collection.GenSetLike 
  def iterator: Iterator[Int] = {
    lower.iterator.flatMap(_.iterator) ++ interval.range.iterator ++ upper.iterator.flatMap(_.iterator)
  }
  // Members declared in scala.collection.SetLike 
  def -(elem: Int): Intervals = this - Interval(elem, elem)

  def +(elem: Int): Intervals = this + Interval(elem, elem)

  def contains(elem: Int): Boolean = {
    if (interval.contains(elem)) {
      true
    } else if (elem < interval.lb) {
      lower.exists(_.contains(elem))
    } else if (elem > interval.ub) {
      upper.exists(_.contains(elem))
    } else {
      false
    }
  }

  override def toString = asSequence.mkString(" + ")

  // Members declared in  scala.collection.generic.Sorted 
  def keysIteratorFrom(start: Int): Iterator[Int] = ???
  // Members declared in scala.collection.SortedSetLike 
  implicit def ordering: Ordering[Int] = Ordering.Int

  def rangeImpl(from: Option[Int], until: Option[Int]): SortedSet[Int] = ???
}