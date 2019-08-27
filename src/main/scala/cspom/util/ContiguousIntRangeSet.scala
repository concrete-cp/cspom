package cspom.util

import scala.collection.immutable.SortedSet

final class ContiguousIntRangeSet(val r: RangeSet[Infinitable]) extends SortedSet[BigInt] {

  def ordering: Ordering[BigInt] = Ordering.BigInt

  def allValues(i: Interval[Infinitable]): Iterable[BigInt] = i.asInstanceOf[IntInterval]

  def iterator: Iterator[BigInt] = r.contents.iterator.flatMap(allValues)

  override def isEmpty: Boolean = r.isEmpty

  def excl(elem: BigInt): SortedSet[BigInt] =
    new ContiguousIntRangeSet(r -- IntInterval.singleton(elem))

  def incl(elem: BigInt): SortedSet[BigInt] =
    new ContiguousIntRangeSet(r ++ IntInterval.singleton(elem))

  def contains(elem: Infinitable): Boolean = r.intersects(IntInterval.singleton(elem))

  def contains(elem: BigInt): Boolean = contains(Finite(elem))

  def contains(elem: Int): Boolean = contains(BigInt(elem))

  def iteratorFrom(start: BigInt): Iterator[BigInt] =
    (r & IntInterval.atLeast(start)).contents.iterator.flatMap(allValues)

  def rangeImpl(from: Option[BigInt], until: Option[BigInt]): SortedSet[BigInt] = {
    val i = from.map(IntInterval.atLeast).getOrElse(IntInterval.all) &
      until.map(IntInterval.atMost).getOrElse(IntInterval.all)

    new ContiguousIntRangeSet(r & i)
  }

  override def last: BigInt = r.upperBound match {
    case Finite(u) => u
    case _         => throw new IllegalStateException(s"$r is not finite")
  }

  def totalSize: Infinitable = r.contents.view.map(_.itvSize).foldLeft(Finite(0): Infinitable)(_ + _)

  override def size: Int = Math.toIntExact(totalSize.finite)

  def singletonMatch: Option[BigInt] = {
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