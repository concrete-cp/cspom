package cspom.util

import scala.collection.JavaConversions
import com.google.common.collect.{BoundType => GuavaBT}
import com.google.common.collect.ContiguousSet
import com.google.common.collect.DiscreteDomain
import com.google.common.collect.Range
import cspom.util.GuavaRange.AsOrdered

object BoundType {
  def apply(t: GuavaBT) = t match {
    case GuavaBT.CLOSED => Closed
    case GuavaBT.OPEN => Open
  }

  val closedIsLess = Ordering.fromLessThan[BoundType] {
    case (Closed, Open) => true
    case _ => false
  }

  val closedIsMore = Ordering.fromLessThan[BoundType] {
    case (Open, Closed) => true
    case _ => false
  }
}

sealed trait BoundType {
  def other: BoundType
  def guava: GuavaBT
  def &(bt: BoundType): BoundType
}

case object Closed extends BoundType {
  def other = Open
  def guava = GuavaBT.CLOSED
  def &(bt: BoundType) = bt match {
    case Closed => Closed
    case Open => Open
  }
}
case object Open extends BoundType {
  def other = Closed
  def guava = GuavaBT.OPEN
  def &(bt: BoundType) = Open

}

object GuavaRange {

  implicit class AsOrdered[T <% Ordered[T]](val value: T) extends Ordered[AsOrdered[T]] {
    override def compare(that: AsOrdered[T]) = value.compare(that.value)
    override def toString = value.toString

  }

  def all[A <% Ordered[A]](): GuavaRange[A] = GuavaRange(Range.all[AsOrdered[A]]())

  def atLeast[A <% Ordered[A]](endpoint: A) = GuavaRange(Range.atLeast(AsOrdered(endpoint)))

  def atMost[A <% Ordered[A]](endpoint: A) = GuavaRange(Range.atLeast(AsOrdered(endpoint)))

  def closed[A <% Ordered[A]](lower: A, upper: A) = GuavaRange(Range.closed[AsOrdered[A]](lower, upper))

  def closedOpen[A <% Ordered[A]](lower: A, upper: A) = GuavaRange(Range.closedOpen[AsOrdered[A]](lower, upper))

  def greaterThan[A <% Ordered[A]](endpoint: A) = GuavaRange(Range.greaterThan[AsOrdered[A]](endpoint))

  def lessThan[A <% Ordered[A]](endpoint: A) = GuavaRange(Range.lessThan[AsOrdered[A]](endpoint))

  def open[A <% Ordered[A]](lower: A, upper: A) = GuavaRange(Range.open[AsOrdered[A]](lower, upper))

  def openClosed[A <% Ordered[A]](lower: A, upper: A) = GuavaRange(Range.openClosed[AsOrdered[A]](lower, upper))

  def upTo[A <% Ordered[A]](endpoint: A, bt: BoundType) = GuavaRange(
    Range.upTo[AsOrdered[A]](endpoint, bt.guava))

  def downTo[A <% Ordered[A]](endpoint: A, bt: BoundType) = GuavaRange(
    Range.downTo[AsOrdered[A]](endpoint, bt.guava))

  def singleton[A <% Ordered[A]](v: A) = GuavaRange(Range.singleton[AsOrdered[A]](v))

  def apply[A <% Ordered[A]](lower: A, lowerType: BoundType, upper: A, upperType: BoundType): GuavaRange[A] =
    GuavaRange(Range.range(lower, lowerType.guava, upper, upperType.guava))
}

final case class GuavaRange[A <% Ordered[A]](
  val r: Range[AsOrdered[A]]) {

  def contains(c: A) = r.contains(c)

  def hasLowerBound = r.hasLowerBound()
  def hasUpperBound = r.hasUpperBound()

  def &(si: GuavaRange[A]) = GuavaRange(r.intersection(si.r))
  def isConnected(si: GuavaRange[A]) = r.isConnected(si.r)
  def span(si: GuavaRange[A]) = GuavaRange(r.span(si.r))

  def isEmpty = r.isEmpty()

  def lowerEndpoint = r.lowerEndpoint().value
  def upperEndpoint = r.upperEndpoint().value

  def upperBoundType = BoundType(r.upperBoundType())
  def lowerBoundType = BoundType(r.lowerBoundType())

  def isBefore(h: GuavaRange[A]): Boolean = {
    isBefore(h.lowerEndpoint)
  }

  def isBefore(elem: A): Boolean = {
    !contains(elem) && upperEndpoint < elem
  }

  def isAfter(h: GuavaRange[A]): Boolean = {
    isAfter(h.upperEndpoint)
  }

  def isAfter(elem: A): Boolean = {
    !contains(elem) && elem < lowerEndpoint
  }

  def canonical(implicit d: DiscreteDomain[AsOrdered[A]]): GuavaRange[A] =
    GuavaRange(r.canonical(d))

  def allValues(implicit d: DiscreteDomain[AsOrdered[A]]): Iterator[A] =
    JavaConversions.asScalaIterator(ContiguousSet.create(r, d).iterator).map(_.value)

  override def toString = r.toString

}