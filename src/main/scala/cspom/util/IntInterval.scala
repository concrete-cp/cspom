package cspom.util

import com.typesafe.scalalogging.LazyLogging
import cspom.util.Infinitable.InfinitableOrdering

import scala.collection.immutable.NumericRange

object IntInterval {
  val all: IntInterval = IntInterval(MinInf, PlusInf)

  def atLeast(endpoint: BigInt) = IntInterval(Finite(endpoint), PlusInf)

  def atMost(endpoint: BigInt) = IntInterval(MinInf, Finite(endpoint))

  def apply(lower: BigInt, upper: BigInt): IntInterval =
    IntInterval(Finite(lower), Finite(upper))

  def apply(lower: Infinitable, upper: Infinitable): IntInterval =
    new IntInterval(lower, upper)

  def singleton(v: Int): IntInterval = singleton(BigInt(v))

  def singleton(v: BigInt): IntInterval = {
    singleton(Finite(v))
  }

  def singleton(v: Infinitable): IntInterval = IntInterval(v, v)

}

object FiniteIntInterval {
  def unapply(itv: IntInterval): Option[(BigInt, BigInt)] = Interval.unapply(itv).collect {
    case (Finite(l), Finite(u)) => (l, u)
  }
}

final class IntInterval(
                         val lb: Infinitable, val ub: Infinitable)


  extends Interval[Infinitable] with Iterable[BigInt]
    with LazyLogging {

  require((lb != MinInf || ub != MinInf) && (lb != PlusInf || ub != PlusInf), s"Interval $this is not allowed")


  def iterator: Iterator[BigInt] = asRange.iterator

  def asRange: NumericRange[BigInt] = (lb, ub) match {
    case (Finite(l), Finite(u)) => l to u
    case _ => throw new ArithmeticException(s"Cannot create infinite range $this ")
  }

  def itvSize: Infinitable = {
    val s = Finite(1) + ub - lb
    if (s <= 0) Finite(0) else s
  }

  def contains(c: BigInt): Boolean = lb <= c && !(ub < c)

  def &(si: Interval[Infinitable]): Interval[Infinitable] = {

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

  def lessThan(siub: Infinitable): IntInterval = {
    if (siub == MinInf) {
      // Force empty interval
      IntInterval(PlusInf, MinInf)
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


  def moreThan(silb: Infinitable): IntInterval = {
    if (silb == PlusInf) {
      // Force empty interval
      IntInterval(PlusInf, MinInf)
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

  def span(si: Interval[Infinitable]): Interval[Infinitable] = {
    val spanned = span(si.lb, si.ub)
    if (spanned == si) si else spanned
  }

  def span(silb: Infinitable, siub: Infinitable): IntInterval = {
    val lowerCmp = InfinitableOrdering.compare(lb, silb)
    val upperCmp = InfinitableOrdering.compare(ub, siub)
    if (lowerCmp <= 0 && upperCmp >= 0) {
      this
    } else {
      val newLower = if (lowerCmp <= 0) lb else silb
      val newUpper = if (upperCmp >= 0) ub else siub
      IntInterval(newLower, newUpper)
    }
  }

  override def isEmpty: Boolean = {
    InfinitableOrdering.compare(lb, ub) > 0
  }

  override def size: Int = Math.toIntExact(itvSize.finite)

  def isBefore(h: Interval[Infinitable]): Boolean = {
    h.lb match {
      case MinInf => false
      case Finite(i) => isBefore(i)
      case PlusInf => throw new ArithmeticException(s"Interval $h lb is incorrect")
    }
  }

  def isBefore(elem: BigInt): Boolean = {
    ub match {
      case PlusInf => false
      case Finite(i) => i < elem - 1
      case MinInf => throw new ArithmeticException(s"Interval $this ub is incorrect")
    }
  }

  def isAfter(h: Interval[Infinitable]): Boolean = {
    h.ub match {
      case PlusInf => false
      case Finite(i) => isAfter(i)
      case MinInf => throw new ArithmeticException(s"Interval $h ub is incorrect")
    }
  }

  def isAfter(elem: BigInt): Boolean = {
    lb match {
      case MinInf => false
      case Finite(i) =>
        i > elem + 1
      case PlusInf => throw new ArithmeticException(s"Interval $this lb is incorrect")
    }
  }

  //  override def equals(o: Any) = {
  //    o match {
  //      case Interval(l, u) => lb == l && ub == u
  //      case _ => super.equals(o)
  //    }
  //  }
  //
  //  override def hashCode = (lb, ub).hashCode

  override def toString: String = {
    if (lb == ub) {
      s"{$lb}"
    } else {
      val l = lb match {
        case MinInf => s"($MinInf"
        case Finite(i) => s"[$i"
        case PlusInf => s"($PlusInf"
      }
      val u = ub match {
        case Finite(i) => s"$i]"
        case PlusInf => s"$PlusInf)"
        case MinInf => s"$MinInf)"
      }

      s"$l‥$u"
    }
  }

}