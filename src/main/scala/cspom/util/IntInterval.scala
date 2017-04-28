package cspom.util

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

  def singleton(v: Infinitable): IntInterval = IntInterval(v, v)

  def singleton(v: Int): IntInterval = {
    singleton(Finite(v))
  }

}

object FiniteIntInterval {
  def unapply(itv: IntInterval) = Interval.unapply(itv).collect {
    case (Finite(l), Finite(u)) => (l, u)
  }
}

final class IntInterval(
  val lb: Infinitable, val ub: Infinitable)
    extends Interval[Infinitable] with Iterable[Int]
    with LazyLogging {

  def iterator: Iterator[Int] = {
    val from = finiteLb
    //val to = Math.checkedAdd(finiteUb, 1)
    //Iterator.from(from).takeWhile(_ <= finiteUb) // => ??? }range(from, to) //.map(Math.checkedAdd(finiteLb, _))
    (from to finiteUb).iterator
  }

  def itvSize = {
    val s = Finite(1) + ub - lb
    if (s <= 0) Finite(0) else s
  }

  def contains(c: Int) = lb <= c && !(ub < c)

  def &(si: Interval[Infinitable]) = {

    val lowerCmp = InfinitableOrdering.compare(lb, si.lb)
    val upperCmp = InfinitableOrdering.compare(ub, si.ub)
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
      val upperCmp = InfinitableOrdering.compare(ub, rsiub)
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
      val lowerCmp = InfinitableOrdering.compare(lb, rsilb)
      if (lowerCmp >= 0) {
        this
      } else {
        IntInterval(rsilb, ub)
      }
    }
  }

  def span(si: Interval[Infinitable]) = {
    val spanned = span(si.lb, si.ub)
    if (spanned == si) si else spanned
  }

  def span(silb: Infinitable, siub: Infinitable) = {
    val lowerCmp = InfinitableOrdering.compare(lb, silb)
    val upperCmp = InfinitableOrdering.compare(ub, siub)
    if (lowerCmp <= 0 && upperCmp >= 0) {
      this
    } else {
      val newLower = if (lowerCmp <= 0) lb else silb
      val newUpper = if (upperCmp >= 0) ub else siub
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
      InfinitableOrdering.compare(lb, ub) > 0
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

//  override def equals(o: Any) = {
//    o match {
//      case Interval(l, u) => lb == l && ub == u
//      case _ => super.equals(o)
//    }
//  }
//
//  override def hashCode = (lb, ub).hashCode

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