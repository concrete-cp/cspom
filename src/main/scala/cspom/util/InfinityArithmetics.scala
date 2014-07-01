package cspom.util

import com.google.common.math.IntMath
import java.math.RoundingMode
import scala.math.Ordering.IntOrdering

object Infinitable {
  implicit object InfinitableOrdering extends Ordering[Infinitable] {
    def compare(i: Infinitable, j: Infinitable): Int = {
      i match {
        case MinInf => -1
        case PlusInf => 1
        case Finite(i) => -Infinitable.compare(j, i)
      }
    }
  }

  def compare(i: Infinitable, j: Int): Int = {
    i match {
      case MinInf => -1
      case PlusInf => 1
      case Finite(i) => Ordering.Int.compare(i, j)
    }
  }
}

sealed trait Infinitable extends Any {
  def +(v: Infinitable): Infinitable
  def -(v: Infinitable): Infinitable
  def *(v: Infinitable): Infinitable
  def div(v: Infinitable, rm: RoundingMode): Infinitable
  def divisible(v: Infinitable): Boolean
  def unary_-(): Infinitable
  def isInfinity = (this == MinInf) || (this == PlusInf)

  def <=(i: Int): Boolean
  def <(i: Int): Boolean
}
case object MinInf extends Infinitable {
  def +(v: Infinitable) = MinInf
  def -(v: Infinitable) = MinInf
  def *(v: Infinitable) = {
    val comp = Infinitable.InfinitableOrdering.compare(v, Finite(0))
    if (comp > 0) {
      MinInf
    } else if (comp < 0) {
      PlusInf
    } else {
      throw new AssertionError("Infinity * 0 is undefined")
    }
  }
  def div(v: Infinitable, rm: RoundingMode) = this * v
  def divisible(v: Infinitable) = false
  def unary_-() = PlusInf
  def <=(i: Int) = true
  def <(i: Int) = true
  override def toString = "-∞"
}
case object PlusInf extends Infinitable {
  def +(v: Infinitable) = PlusInf
  def -(v: Infinitable) = PlusInf
  def *(v: Infinitable) = -(MinInf * v)
  def div(v: Infinitable, rm: RoundingMode) = -(MinInf.div(v, rm))
  def unary_-() = MinInf
  def divisible(v: Infinitable) = false
  def <=(i: Int) = false
  def <(i: Int) = false
  override def toString = "+∞"
}
case class Finite(i: Int) extends AnyVal with Infinitable {
  def +(v: Infinitable) = v match {
    case Finite(j) => Finite(IntMath.checkedAdd(i, j))
    case u => u + this
  }
  def -(v: Infinitable) = v match {
    case Finite(j) => Finite(IntMath.checkedSubtract(i, j))
    case u => u - this
  }
  def *(v: Infinitable) = v match {
    case Finite(j) => Finite(IntMath.checkedMultiply(i, j))
    case u => u * this
  }
  def div(v: Infinitable, rm: RoundingMode) = v match {
    case Finite(j) => Finite(IntMath.divide(i, j, rm))
    case u => Finite(0)
  }
  def divisible(v: Infinitable) = v match {
    case Finite(j) => i % j == 0
    case u => false
  }
  def unary_-() = Finite(-i)

  def <=(j: Int) = i <= j
  def <(j: Int) = i < j
  override def toString = i.toString
}
