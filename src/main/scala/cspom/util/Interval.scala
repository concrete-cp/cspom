package cspom.util

import scala.collection.JavaConversions
import com.google.common.collect.{ BoundType => GuavaBT }
import com.google.common.collect.ContiguousSet
import com.google.common.collect.DiscreteDomain
import com.google.common.collect.Range
import cspom.util.Interval.AsOrdered
import com.typesafe.scalalogging.slf4j.LazyLogging

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

object Interval {

  implicit class AsOrdered[T <% Ordered[T]](val value: T) extends Ordered[AsOrdered[T]] {
    override def compare(that: AsOrdered[T]) = value.compare(that.value)
    override def toString = value.toString

  }

  class Closing[A <% Ordered[A]](left: A) {
    def -(right: A) = new {
      def > = Interval(left, right)
      def < = Interval.closedOpen(left, right)
    }

    def > = Interval.singleton(left)
  }

  class Opening[A <% Ordered[A]](left: A) {
    def -(right: A) = new {
      def > = Interval.openClosed(left, right)
      def < = Interval.open(left, right)
    }

  }

  def all[A <% Ordered[A]](): Interval[A] = Interval(Range.all[AsOrdered[A]]())

  def atLeast[A <% Ordered[A]](endpoint: A) = Interval(Range.atLeast(AsOrdered(endpoint)))

  def atMost[A <% Ordered[A]](endpoint: A) = Interval(Range.atMost(AsOrdered(endpoint)))

  def apply[A <% Ordered[A]](lower: A, upper: A): Interval[A] =
    Interval(Range.closed[AsOrdered[A]](lower, upper))

  def closedOpen[A <% Ordered[A]](lower: A, upper: A) = Interval(Range.closedOpen[AsOrdered[A]](lower, upper))

  def greaterThan[A <% Ordered[A]](endpoint: A) = Interval(Range.greaterThan[AsOrdered[A]](endpoint))

  def lessThan[A <% Ordered[A]](endpoint: A) = Interval(Range.lessThan[AsOrdered[A]](endpoint))

  def open[A <% Ordered[A]](lower: A, upper: A) = Interval(Range.open[AsOrdered[A]](lower, upper))

  def openClosed[A <% Ordered[A]](lower: A, upper: A) = Interval(Range.openClosed[AsOrdered[A]](lower, upper))

  def upTo[A <% Ordered[A]](endpoint: A, bt: BoundType) = Interval(
    Range.upTo[AsOrdered[A]](endpoint, bt.guava))

  def downTo[A <% Ordered[A]](endpoint: A, bt: BoundType) = Interval(
    Range.downTo[AsOrdered[A]](endpoint, bt.guava))

  def apply[A <% Ordered[A]](lower: A, lowerType: BoundType, upper: A, upperType: BoundType): Interval[A] =
    Interval(Range.range(lower, lowerType.guava, upper, upperType.guava))

  def singleton[A <% Ordered[A]](v: A): Interval[A] =
    Interval(Range.singleton[AsOrdered[A]](v))

  def <[A <% Ordered[A]](left: A) = new Closing[A](left)

  def >[A <% Ordered[A]](left: A) = new Opening[A](left)

  val r = <(2) - (5)>
}

final case class Interval[A <% Ordered[A]](
  val r: Range[AsOrdered[A]]) extends LazyLogging {

  def contains(c: A) = r.contains(c)

  def hasLowerBound = r.hasLowerBound()
  def hasUpperBound = r.hasUpperBound()

  def &(si: Interval[A]) = Interval(r.intersection(si.r))
  def isConnected(si: Interval[A]) = r.isConnected(si.r)
  def span(si: Interval[A]) = Interval(r.span(si.r))

  def isEmpty = r.isEmpty()

  def lowerEndpoint = r.lowerEndpoint().value
  def upperEndpoint = r.upperEndpoint().value

  def lowerBoundOption: Option[(A, BoundType)] =
    if (hasLowerBound) {
      Some((lowerEndpoint, lowerBoundType))
    } else {
      None
    }

  def upperBoundOption: Option[(A, BoundType)] =
    if (hasUpperBound) {
      Some((upperEndpoint, upperBoundType))
    } else {
      None
    }

  def upperBoundType = BoundType(r.upperBoundType())
  def lowerBoundType = BoundType(r.lowerBoundType())

  def isBefore(h: Interval[A]): Boolean = {
    isBefore(h.lowerEndpoint)
  }

  def isBefore(elem: A): Boolean = {
    !contains(elem) && upperEndpoint < elem
  }

  def isAfter(h: Interval[A]): Boolean = {
    isAfter(h.upperEndpoint)
  }

  def isAfter(elem: A): Boolean = {
    !contains(elem) && elem < lowerEndpoint
  }

  def canonical(implicit d: DiscreteDomain[AsOrdered[A]]): Interval[A] = {
    if (hasLowerBound && lowerBoundType == Open && d.next(lowerEndpoint) == null) {
      val m = d.maxValue().value
      logger.warn(s"Range $r's canonical form forced to [$m, $m)")
      Interval.closedOpen(m, m)
    } else {
      Interval(r.canonical(d))
    }
  }

  def allValues(implicit d: DiscreteDomain[AsOrdered[A]]): Iterator[A] =
    JavaConversions.asScalaIterator(ContiguousSet.create(r, d).iterator).map(_.value)

  def firstValue(implicit d: DiscreteDomain[AsOrdered[A]]): A = {
    ContiguousSet.create(r, d).first().value
  }

  def lastValue(implicit d: DiscreteDomain[AsOrdered[A]]): A = {
    ContiguousSet.create(r, d).last().value
  }

  def nbValues(implicit d: DiscreteDomain[AsOrdered[A]]): Int = {
    ContiguousSet.create(r, d).size
  }

  override def toString = r.toString

}