package cspom.util

import java.math.RoundingMode

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

  def isInfinity: Boolean = (this == MinInf) || (this == PlusInf)

  def <=(i: BigInt): Boolean

  def <(i: BigInt): Boolean

  def >=(i: BigInt): Boolean

  def >(i: BigInt): Boolean

  def abs: Infinitable

  def max(v: Infinitable): Infinitable = Infinitable.InfinitableOrdering.max(this, v)

  def min(v: Infinitable): Infinitable = Infinitable.InfinitableOrdering.min(this, v)

  def pow(v: Infinitable): Infinitable
}

case object MinInf extends Infinitable {
  def pow(b: Infinitable) = {
    b match {
      case Finite(b) => if (b % 2 == 0) PlusInf else MinInf
      case _ => throw new ArithmeticException(s"pow($this, $b) is undefined")
    }
  }

  def +(v: Infinitable) = MinInf

  def -(v: Infinitable) = {
    if (v == MinInf) {
      throw new ArithmeticException("-Infinity - -Infinity is undefined")
    } else {
      MinInf
    }
  }

  def div(v: Infinitable, rm: RoundingMode) = this * v

  def *(v: Infinitable) = {
    val comp = Infinitable.InfinitableOrdering.compare(v, Finite(0))
    if (comp > 0) {
      MinInf
    } else if (comp < 0) {
      PlusInf
    } else {
      throw new ArithmeticException("Infinity * 0 is undefined")
    }
  }

  def divisible(v: Infinitable) = false

  def unary_-() = PlusInf

  def <=(i: BigInt) = true

  def <(i: BigInt) = true

  def >=(i: BigInt) = false

  def >(i: BigInt) = false

  override def toString = "-∞"

  def abs = PlusInf
}

case object PlusInf extends Infinitable {
  def +(v: Infinitable) = PlusInf

  def -(v: Infinitable) = {
    if (v == PlusInf) {
      throw new ArithmeticException("Infinity - Infinity is undefined")
    } else {
      PlusInf
    }
  }

  def *(v: Infinitable) = -(MinInf * v)

  def div(v: Infinitable, rm: RoundingMode) = -MinInf.div(v, rm)

  def unary_-() = MinInf

  def divisible(v: Infinitable) = false

  def <=(i: BigInt) = false

  def <(i: BigInt) = false

  def >=(i: BigInt) = true

  def >(i: BigInt) = true

  override def toString = "+∞"

  def abs = PlusInf

  def pow(b: Infinitable) = {
    b match {
      case MinInf => throw new ArithmeticException(s"pow($this, $b) is undefined")
      case _ => PlusInf
    }
  }
}

case class Finite(i: Int) extends AnyVal with Infinitable {
  def +(v: Infinitable) = v match {
    case Finite(j) => Finite(Math.checkedAdd(i, j))
    case u => u + this
  }

  def -(v: Infinitable) = v match {
    case Finite(j) => Finite(Math.checkedSubtract(i, j))
    case u => -u + this
  }

  def *(v: Infinitable) = v match {
    case Finite(j) => Finite(Math.checkedMultiply(i, j))
    case u => u * this
  }

  def div(v: Infinitable, rm: RoundingMode) = v match {
    case Finite(j) => Finite(Math.divide(i, j, rm))
    case u => Finite(0)
  }

  def divisible(v: Infinitable) = v match {
    case Finite(j) => i % j == 0
    case _ => false
  }

  def unary_-() = Finite(-i)

  def <=(j: BigInt) = i <= j

  def <(j: BigInt) = i < j

  def >=(j: BigInt) = i >= j

  def >(j: BigInt) = i > j

  override def toString = i.toString

  def abs = Finite(math.abs(i))

  def pow(b: Infinitable) = {
    b match {
      case Finite(b) => Finite(Math.checkedPow(i, b))
      case PlusInf => PlusInf
      case MinInf => Finite(0)
    }
  }
}
