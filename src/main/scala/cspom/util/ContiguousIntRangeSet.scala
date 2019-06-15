package cspom.util

import scala.collection.immutable.SortedSet

final class ContiguousIntRangeSet(val r: RangeSet[Infinitable]) extends SortedSet[Int] {

  def ordering: Ordering[Int] = Ordering.Int

  def allValues(i: Interval[Infinitable]): Iterable[Int] = i.asInstanceOf[IntInterval]

  def iterator: Iterator[Int] = r.contents.iterator.flatMap(allValues)

  override def foreach[U](f: Int => U): Unit = {
    for (rs <- r.contents) {
      rs.asInstanceOf[IntInterval].foreach(f)
    }
  }

  override def isEmpty: Boolean = r.isEmpty

  def excl(elem: Int): SortedSet[Int] =
    new ContiguousIntRangeSet(r -- IntInterval.singleton(elem))

  def incl(elem: Int): SortedSet[Int] =
    new ContiguousIntRangeSet(r ++ IntInterval.singleton(elem))

  def contains(elem: Infinitable): Boolean = r.intersects(IntInterval.singleton(elem))

  def contains(elem: Int): Boolean = contains(Finite(elem))

  def iteratorFrom(start: Int): Iterator[Int] =
    (r & IntInterval.atLeast(start)).contents.iterator.flatMap(allValues)

  def rangeImpl(from: Option[Int], until: Option[Int]): SortedSet[Int] = {
    val i = from.map(IntInterval.atLeast).getOrElse(IntInterval.all) &
      until.map(IntInterval.atMost).getOrElse(IntInterval.all)

    new ContiguousIntRangeSet(r & i)
  }

  override def last: Int = r.upperBound match {
    case Finite(u) => u
    case _         => throw new IllegalStateException(s"$r is not finite")
  }

  override def size: Int = r.contents.iterator.map(allValues(_).size).sum

  def singletonMatch: Option[Int] = {
    if (r.isEmpty) {
      None
    } else if (r.fullyDefined) {
      val Finite(l) = r.lowerBound
      val Finite(u) = r.upperBound
      if (l == u) {
        Some(l)
      } else {
        None
      }
    } else {
      None
    }
  }
}