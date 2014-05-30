package cspom.util

import com.google.common.collect.ContiguousSet
import com.google.common.collect.DiscreteDomain
import scala.collection.JavaConversions
import cspom.util.Interval.AsOrdered

object IntDiscreteDomain extends DiscreteDomain[AsOrdered[Int]]
  with Serializable {

  def next(value: AsOrdered[Int]) = {
    val i = value.value;
    if (i == Int.MaxValue) null else i + 1
  }

  def previous(value: AsOrdered[Int]) = {
    val i = value.value;
    if (i == Int.MinValue) null else i - 1;
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

}
