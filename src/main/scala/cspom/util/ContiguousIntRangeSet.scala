package cspom.util

import scala.collection.immutable.SortedSet
import com.google.common.collect.DiscreteDomain

final class ContiguousIntRangeSet(val r: IntRangeSet) extends SortedSet[Int] {

  def ordering = Ordering.Int

  def iterator: Iterator[Int] =
    r.ranges.iterator.flatMap(_.allValues)

  def -(elem: Int): SortedSet[Int] =
    new ContiguousIntRangeSet(r -- IntInterval.singleton(elem))

  def +(elem: Int): SortedSet[Int] =
    new ContiguousIntRangeSet(r ++ IntInterval.singleton(elem))

  def contains(elem: Int): Boolean = r.contains(elem)

  def keysIteratorFrom(start: Int): Iterator[Int] =
    (r & IntInterval.atLeast(start)).ranges.iterator.flatMap(_.allValues)

  def rangeImpl(from: Option[Int], until: Option[Int]): SortedSet[Int] = {
    val i = from.map(IntInterval.atLeast(_)).getOrElse(IntInterval.all) &
      until.map(IntInterval.atMost(_)).getOrElse(IntInterval.all)

    new ContiguousIntRangeSet(r & i)
  }

  override def last = {
    val Finite(u) = r.upperBound
    u
  }

  override def size = r.ranges.iterator.map(_.nbValues).sum

  def singletonMatch: Option[Int] = {
    if (r.fullyDefined) {
      toStream match {
        case Stream(c) => Some(c)
        case _ => None
      }
    } else {
      None
    }
  }
}