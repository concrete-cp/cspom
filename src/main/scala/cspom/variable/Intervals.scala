package cspom.variable

import scala.collection.immutable.SortedSet
import scala.collection.immutable.VectorBuilder

object Intervals {
  def apply(s: Iterable[Int]): Intervals = s match {
    case r: Range if r.step == 1 => Intervals(r.head, r.last)
    case i: Intervals => i
    case s => s.foldLeft(empty)(_ + _)
  }

  def apply(itv: Interval): Intervals = new Intervals(IndexedSeq(itv))

  def apply(lb: Int, ub: Int): Intervals = Intervals(Interval(lb, ub))

  def apply() = empty

  val empty: Intervals = new Intervals(IndexedSeq())
}

final class Intervals private (
  val intervals: IndexedSeq[Interval]) extends SortedSet[Int] {

  require(intervals.forall(_.nonEmpty))

  def ++(i: Intervals): Intervals = {
    if (isEmpty) {
      i
    } else if (i.isEmpty) {
      this
    } else {

      val vb = new VectorBuilder[Interval]()

      var itv1: IndexedSeq[Interval] = null
      var itv2: IndexedSeq[Interval] = null

      def update(i: IndexedSeq[Interval], j: IndexedSeq[Interval]) {
        if (i.head.lb < j.head.lb) {
          itv1 = i
          itv2 = j
        } else {
          itv1 = j
          itv2 = i
        }
      }

      update(this.intervals, i.intervals)

      var current = itv1.head
      itv1 = itv1.tail

      while (itv1.nonEmpty && itv2.nonEmpty) {
        if (itv1.head isBefore current) {
          vb += current
          current = itv1.head
          update(itv1.tail, itv2)
        } else {
          current = current union itv1.head
          update(itv1.tail, itv2)
        }
      }

      if (itv1.isEmpty) {
        itv1 = itv2
      }

      while (itv1.headOption.exists(!_.isBefore(current))) {
        current = current union itv1.head
        itv1 = itv1.tail
      }

      vb += current
      vb ++= itv1

      new Intervals(vb.result)
    }
  }

  def +(i: Interval): Intervals = this ++ Intervals(i)

  override val size: Int = {
    val l = intervals.foldLeft(0l)(_ + _.size)
    require(l <= Int.MaxValue)
    l.toInt
  }

  def convex = intervals.size <= 1

  def max: Int = intervals.last.ub

  def min: Int = intervals.head.ub

  override def equals(o: Any): Boolean = {
    o match {
      case i: Intervals => i.intervals == intervals
      case s => super.equals(s)
    }
  }

  def -(i: Interval): Intervals = this -- Intervals(i)

  def --(i: Intervals): Intervals = ???

  def intersect(i: Intervals): Intervals = this -- (this -- i)

  def intersect(i: Interval): Intervals = this -- (this - i)

  // Members declared in scala.collection.GenSetLike 
  def iterator: Iterator[Int] = intervals.iterator.flatMap(_.range.iterator)

  // Members declared in scala.collection.SetLike 
  def -(elem: Int): Intervals = this - Interval(elem, elem)

  def +(elem: Int): Intervals = this + Interval(elem, elem)

  def contains(elem: Int): Boolean = ???

  override def toString = intervals.mkString(" + ")

  // Members declared in  scala.collection.generic.Sorted 
  def keysIteratorFrom(start: Int): Iterator[Int] = ???
  // Members declared in scala.collection.SortedSetLike 
  implicit def ordering: Ordering[Int] = Ordering.Int

  def rangeImpl(from: Option[Int], until: Option[Int]): SortedSet[Int] = ???
}