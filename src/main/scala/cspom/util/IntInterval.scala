package cspom.util

import scala.collection.AbstractIterator

import com.typesafe.scalalogging.LazyLogging

import Infinitable.InfinitableOrdering

object IntInterval {
  val all: IntInterval = IntInterval(MinInf, PlusInf)

  def atLeast(endpoint: Int) = IntInterval(Finite(endpoint), PlusInf)

  def atMost(endpoint: Int) = IntInterval(MinInf, Finite(endpoint))

  def apply(lower: Int, upper: Int): IntInterval =
    IntInterval(Finite(lower), Finite(upper))

  def apply(lower: Infinitable, upper: Infinitable): IntInterval =
    new IntInterval(lower, upper)

  implicit def singleton(v: Int): IntInterval = IntInterval(v, v)

  val ordering = new IntIntervalOrdering[Infinitable]

}

final class IntInterval(
  val lb: Infinitable, val ub: Infinitable)
  extends Interval[Infinitable] with Iterable[Int]
  with LazyLogging {

  //require(lb != PlusInf)
  //require(ub != MinInf)

  def compare(i: Infinitable, j: Infinitable) = InfinitableOrdering.compare(i, j)

  def iterator: Iterator[Int] = {
    val finiteLb = this.finiteLb
    (0 until size).map(Math.checkedAdd(finiteLb, _)).iterator
  }

  def contains(c: Infinitable) = compare(lb, c) <= 0 && compare(ub, c) >= 0

  def contains(c: Int) = lb <= c && !(ub < c)

  def &(si: Interval[Infinitable]) = {

    val lowerCmp = compare(lb, si.lb)
    val upperCmp = compare(ub, si.ub)
    if (lowerCmp >= 0 && upperCmp <= 0) {
      this
    } else if (lowerCmp <= 0 && upperCmp >= 0) {
      si
    } else {
      val newLower = if (lowerCmp >= 0) lb else si.lb
      val newUpper = if (upperCmp <= 0) ub else si.ub
      IntInterval(newLower, newUpper)
    }
  }

  def lessThan(siub: Infinitable) = {
    if (siub <= Int.MinValue) {
      IntInterval(lb, MinInf)
    } else {
      val rsiub = siub - Finite(1)
      val upperCmp = compare(ub, rsiub)
      if (upperCmp <= 0) {
        this
      } else {
        IntInterval(lb, rsiub)
      }
    }
  }

  def moreThan(silb: Infinitable) = {
    if (Infinitable.compare(silb, Int.MaxValue) >= 0) {
      IntInterval(PlusInf, ub)
    } else {
      val rsilb = silb + Finite(1)
      val lowerCmp = compare(lb, rsilb)
      if (lowerCmp >= 0) {
        this
      } else {
        IntInterval(rsilb, ub)
      }
    }
  }

  def isConnected(si: Interval[Infinitable]) = !((this isAfter si) || (this isBefore si))

  def span(si: Interval[Infinitable]) = {
    val lowerCmp = compare(lb, si.lb)
    val upperCmp = compare(ub, si.ub)
    if (lowerCmp <= 0 && upperCmp >= 0) {
      this
    } else if (lowerCmp >= 0 && upperCmp <= 0) {
      si
    } else {
      val newLower = if (lowerCmp <= 0) lb else si.lb
      val newUpper = if (upperCmp >= 0) ub else si.ub
      IntInterval(newLower, newUpper);
    }
  }

  def finiteLb = lb match {
    case Finite(l) => l
    case _ => throw new AssertionError("lb is not finite")
  }

  def finiteUb = ub match {
    case Finite(u) => u
    case _ => throw new AssertionError("ub is not finite")
  }

  override def isEmpty = {
    lb == PlusInf ||
      ub == MinInf ||
      compare(lb, ub) > 0
  }

  override def size: Int = Math.checkedAdd(Math.checkedSubtract(finiteUb, finiteLb), 1)

  def isBefore(h: Interval[Infinitable]): Boolean = {
    h.lb match {
      case MinInf => false
      case Finite(i) => isBefore(i)
      case PlusInf => throw new IllegalArgumentException
    }
  }

  def isBefore(elem: Int): Boolean = {
    elem > Int.MinValue && (
      ub match {
        case PlusInf => false
        case Finite(i) => i < elem - 1
        case MinInf => throw new IllegalArgumentException
      })
  }

  def isAfter(h: Interval[Infinitable]): Boolean = {
    h.ub match {
      case PlusInf => false
      case Finite(i) => isAfter(i)
      case MinInf => throw new IllegalArgumentException
    }
  }

  def isAfter(elem: Int): Boolean = {
    elem < Int.MaxValue && (
      lb match {
        case MinInf => false
        case Finite(i) =>
          i > elem + 1
        case PlusInf => throw new IllegalArgumentException
      })
  }

  override def equals(o: Any) = {
    o match {
      case Interval(l, u) => lb == l && ub == u
      case _ => super.equals(o)
    }
  }

  override def hashCode = {
    41 * lb.hashCode + ub.hashCode
  }

  override def toString = {
    val l = lb match {
      case MinInf => "(-\u221e"
      case Finite(i) => s"[$i"
      case PlusInf => "(+\u221e"
    }
    val u = ub match {
      case Finite(i) => s"$i]"
      case PlusInf => "+\u221e)"
      case MinInf => "-\u221e)"
    }

    s"$l‥$u"
  }

}