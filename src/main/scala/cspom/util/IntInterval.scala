package cspom.util

import com.google.common.collect.{ BoundType => GuavaBT }
import com.google.common.collect.ContiguousSet
import com.google.common.collect.Range
import com.typesafe.scalalogging.slf4j.LazyLogging
import com.google.common.collect.DiscreteDomain
import scala.collection.JavaConversions

object IntInterval {

  class Closing(left: Integer) {
    def -(right: Integer) = new {
      def > = IntInterval(left, right)
      def < = IntInterval.closedOpen(left, right)
    }

    def - = IntInterval.atLeast(left)

    def > = IntInterval.singleton(left)
  }

  class Opening(left: Integer) {
    def -(right: Integer) = new {
      def > = IntInterval.openClosed(left, right)
      def < = IntInterval.open(left, right)
    }

    def - = IntInterval.greaterThan(left)

  }

  def all(): IntInterval = IntInterval(Range.all[Integer]())

  def atLeast(endpoint: Integer) = IntInterval(Range.atLeast(endpoint))

  def atMost(endpoint: Integer) = IntInterval(Range.atMost(endpoint))

  def apply(lower: Integer, upper: Integer): IntInterval =
    IntInterval(Range.closed(lower, upper))

  def closedOpen(lower: Integer, upper: Integer) = IntInterval(Range.closedOpen(lower, upper))

  def greaterThan(endpoint: Integer) = IntInterval(Range.greaterThan(endpoint))

  def lessThan(endpoint: Integer) = IntInterval(Range.lessThan(endpoint))

  def open(lower: Integer, upper: Integer) = IntInterval(Range.open(lower, upper))

  def openClosed(lower: Integer, upper: Integer) = IntInterval(Range.openClosed(lower, upper))

  def upTo(endpoint: Integer, bt: BoundType) = IntInterval(
    Range.upTo(endpoint, bt.guava))

  def downTo(endpoint: Integer, bt: BoundType) = IntInterval(
    Range.downTo(endpoint, bt.guava))

  def apply(lower: Integer, lowerType: BoundType, upper: Integer, upperType: BoundType): IntInterval =
    IntInterval(Range.range(lower, lowerType.guava, upper, upperType.guava))

  def singleton(v: Integer): IntInterval =
    IntInterval(Range.singleton(v))

  def <(left: Integer) = new Closing(left)

  def >(left: Integer) = new Opening(left)

}

final case class IntInterval(
  val r: Range[Integer]) extends LazyLogging {

  def contains(c: Integer) = r.contains(c)

  def hasLowerBound = r.hasLowerBound()
  def hasUpperBound = r.hasUpperBound()

  def &(si: IntInterval) = IntInterval(r.intersection(si.r))
  def isConnected(si: IntInterval) = r.isConnected(si.r)
  def span(si: IntInterval): IntInterval = IntInterval(r.span(si.r))

  def isEmpty = r.isEmpty()

  def lowerEndpoint = r.lowerEndpoint()
  def upperEndpoint = r.upperEndpoint()

  def lowerBoundOption: Option[(Integer, BoundType)] =
    if (hasLowerBound) {
      Some((lowerEndpoint, lowerBoundType))
    } else {
      None
    }

  def upperBoundOption: Option[(Integer, BoundType)] =
    if (hasUpperBound) {
      Some((upperEndpoint, upperBoundType))
    } else {
      None
    }

  def upperBoundType = if (hasUpperBound) BoundType(r.upperBoundType()) else Open
  def lowerBoundType = if (hasLowerBound) BoundType(r.lowerBoundType()) else Open

  def isBefore(h: IntInterval): Boolean = {
    h.hasLowerBound && isBefore(h.lowerEndpoint)
  }

  def isBefore(elem: Integer): Boolean = {
    hasUpperBound && !contains(elem) && upperEndpoint <= elem
  }

  def isAfter(h: IntInterval): Boolean = {
    h.hasUpperBound && isAfter(h.upperEndpoint)
  }

  def isAfter(elem: Integer): Boolean = {
    hasLowerBound && !contains(elem) && elem <= lowerEndpoint
  }

  def canonical: IntInterval = {
    val d = DiscreteDomain.integers
    if (hasLowerBound) {
      if (lowerBoundType == Open && d.next(lowerEndpoint) == null) {

        val m = d.maxValue()
        logger.warn(s"Range $r's canonical form forced to [$m, $m)")
        IntInterval.closedOpen(m, m)
      } else {
        IntInterval(r.canonical(d))
      }
    } else {
      val c = r.canonical(d)
      if (c.hasUpperBound()) {
        IntInterval.lessThan(c.upperEndpoint)
      } else {
        IntInterval.all
      }
    }
  }

  def allValues: Iterator[Integer] =
    JavaConversions.asScalaIterator(ContiguousSet.create(r, DiscreteDomain.integers).iterator)

  def firstValue: Integer = {
    ContiguousSet.create(r, DiscreteDomain.integers).first()
  }

  def lastValue: Integer = {
    ContiguousSet.create(r, DiscreteDomain.integers).last()
  }

  def nbValues: Int = {
    ContiguousSet.create(r, DiscreteDomain.integers).size
  }

  override def toString = r.toString

}