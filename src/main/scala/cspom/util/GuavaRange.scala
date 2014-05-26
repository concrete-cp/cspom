package cspom.util

import com.google.common.collect.Range
import com.google.common.collect.{ BoundType => GuavaBT }
import scala.collection.JavaConversions
import com.google.common.collect.DiscreteDomain
import com.google.common.collect.ContiguousSet
import cspom.util.GuavaRange.AsOrdered

object BoundType {
  def apply(t: GuavaBT) = t match {
    case GuavaBT.CLOSED => Closed
    case GuavaBT.OPEN => Open
  }

  def closedIsLess = Ordering.fromLessThan[BoundType] {
    case (Closed, Open) => true
    case _ => false
  }

  def closedIsMore = Ordering.fromLessThan[BoundType] {
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

  def ofIntInterval(lb: Int, ub: Int) = {
    val u = 1l + ub
    require(u.isValidInt)
    closedOpen(lb, u.toInt)
  }

  def ofInt(v: Int) = ofIntInterval(v, v)

  def apply[A <% Ordered[A]](lower: A, lowerType: BoundType, upper: A, upperType: BoundType): GuavaRange[A] =
    GuavaRange(Range.range(lower, lowerType.guava, upper, upperType.guava))
}

final case class GuavaRange[A <% Ordered[A]](
  val r: Range[AsOrdered[A]]) {

  def contains(c: A) = r.contains(c)
  def containsAll(c: Iterable[A]) = r.containsAll(
    JavaConversions.asJavaCollection(c.view.map(AsOrdered(_))))

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

  override def toString = r.toString

}