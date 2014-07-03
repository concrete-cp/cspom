package cspom.util

import com.google.common.collect.{ BoundType => GuavaBT }
import com.google.common.collect.ContiguousSet
import com.google.common.collect.Range
import com.typesafe.scalalogging.slf4j.LazyLogging
import com.google.common.collect.DiscreteDomain
import scala.collection.JavaConversions
import scala.collection.AbstractIterator
import com.google.common.math.IntMath
import Infinitable.InfinitableOrdering
import InfinitableOrdering.compare

object IntInterval {
  val all: IntInterval = IntInterval(MinInf, PlusInf)

  def atLeast(endpoint: Int) = IntInterval(Finite(endpoint), PlusInf)

  def atMost(endpoint: Int) = IntInterval(MinInf, Finite(endpoint))

  def apply(lower: Int, upper: Int): IntInterval =
    IntInterval(Finite(lower), Finite(upper))

  def unapply(itv: IntInterval): Option[(Infinitable, Infinitable)] = Some((itv.lb, itv.ub))

  def apply(lower: Infinitable, upper: Infinitable): IntInterval = new IntInterval(lower, upper)

  def singleton(v: Int): IntInterval = IntInterval(v, v)

}

final class IntInterval(
  val lb: Infinitable, val ub: Infinitable)
  extends IndexedSeq[Int]
  with LazyLogging {

  require(lb != PlusInf)
  require(ub != MinInf)

  def contains(c: Int) = lb <= c && !(ub < c)

  def &(si: IntInterval) = {

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

  def isConnected(si: IntInterval) = !((this isAfter si) || (this isBefore si))

  def span(si: IntInterval): IntInterval = {
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

  override def isEmpty = compare(lb, ub) > 0

  def apply(idx: Int): Int = IntMath.checkedAdd(finiteLb, idx)

  def length: Int = IntMath.checkedAdd(IntMath.checkedSubtract(finiteUb, finiteLb), 1)

  def isBefore(h: IntInterval): Boolean = {
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

  def isAfter(h: IntInterval): Boolean = {
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

  override def equals(o: Any) = o match {
    case IntInterval(l, u) => lb == l && ub == u
    case _ => super.equals(o)
  }

  override def hashCode = {
    41 * lb.hashCode + ub.hashCode
  }

  override def toString = {
    val l = lb match {
      case MinInf => "(-\u221e"
      case Finite(i) => s"[$i"
      case PlusInf => throw new AssertionError
    }
    val u = ub match {
      case Finite(i) => s"$i]"
      case PlusInf => "+\u221e)"
      case MinInf => throw new AssertionError
    }

    s"$l‥$u"
  }

}