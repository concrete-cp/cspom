package cspom.util

import com.google.common.collect.ContiguousSet
import com.google.common.collect.DiscreteDomain
import scala.collection.JavaConversions
import cspom.util.GuavaRange.AsOrdered

object IntDiscreteDomain extends DiscreteDomain[AsOrdered[Int]]
  with Serializable {

  def next(value: AsOrdered[Int]) = {
    val i = value.value;
    if (i == Int.MaxValue) throw new NoSuchElementException else i + 1
  }

  def previous(value: AsOrdered[Int]) = {
    val i = value.value;
    if (i == Int.MinValue) throw new NoSuchElementException else i - 1;
  }

  def distance(start: AsOrdered[Int], end: AsOrdered[Int]) = {
    end.value.toLong - start.value;
  }

  override def minValue() = {
    Int.MinValue
  }

  override def maxValue() = {
    Int.MaxValue
  }

  def allValues(d: RangeSet[Int]): Iterable[Int] =
    d.ranges.toStream.flatMap(allValues)

  def allValues(r: GuavaRange[Int]): Iterable[Int] = {
    JavaConversions.asScalaIterator(ContiguousSet.create(r.r, IntDiscreteDomain).iterator).
      map(_.value).toStream
  }

  def singleton(d: RangeSet[Int]): Option[Int] = {
    allValues(d) match {
      case Stream(c) => Some(c)
      case _ => None
    }
  }

}
