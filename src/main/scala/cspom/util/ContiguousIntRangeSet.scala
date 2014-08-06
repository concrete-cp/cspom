package cspom.util

import scala.collection.immutable.SortedSet

final class ContiguousIntRangeSet(val r: RangeSet[Infinitable]) extends SortedSet[Int] {

  def ordering = Ordering.Int

  def allValues(i: Interval[Infinitable]): Iterable[Int] = {
    i.asInstanceOf[IntInterval]
  }

  def iterator: Iterator[Int] =
    r.ranges.iterator.flatMap(allValues)

  def -(elem: Int): SortedSet[Int] =
    new ContiguousIntRangeSet(r -- IntInterval.singleton(elem))

  def +(elem: Int): SortedSet[Int] =
    new ContiguousIntRangeSet(r ++ IntInterval.singleton(elem))

  def contains(elem: Infinitable): Boolean = r.contains(elem)

  def contains(elem: Int) = contains(Finite(elem))

  def keysIteratorFrom(start: Int): Iterator[Int] =
    (r & IntInterval.atLeast(start)).ranges.iterator.flatMap(allValues)

  def rangeImpl(from: Option[Int], until: Option[Int]): SortedSet[Int] = {
    val i = from.map(IntInterval.atLeast(_)).getOrElse(IntInterval.all) &
      until.map(IntInterval.atMost(_)).getOrElse(IntInterval.all)

    new ContiguousIntRangeSet(r & i)
  }

  override def last = {
    val Finite(u) = r.upperBound
    u
  }

  override def size = r.ranges.iterator.map(allValues(_).size).sum

  def singletonMatch: Option[Int] = {
    assert(!r.isEmpty)
    if (r.fullyDefined) {
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