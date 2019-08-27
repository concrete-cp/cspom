package cspom.util

import java.math.RoundingMode

object Infinitable {

  implicit object InfinitableOrdering extends Ordering[Infinitable] {
    def compare(i: Infinitable, j: Infinitable): Int = {
      i match {
        case MinInf =>
          if (j == MinInf) 0 else -1
        case PlusInf =>
          if (j == PlusInf) 0 else 1
        case Finite(i) => -Infinitable.compare(j, i)
      }
    }
  }

  def compare(i: Infinitable, j: BigInt): Int = {
    i match {
      case MinInf => -1
      case PlusInf => 1
      case Finite(i) => Ordering.BigInt.compare(i, j)
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

  def finite: BigInt
}

case object MinInf extends Infinitable {
  def pow(b: Infinitable): Infinitable = {
    b match {
      case Finite(b) => if (b % 2 == 0) PlusInf else MinInf
      case _ => throw new ArithmeticException(s"pow($this, $b) is undefined")
    }
  }

  def +(v: Infinitable): MinInf.type = MinInf

  def -(v: Infinitable): MinInf.type = {
    if (v == MinInf) {
      throw new ArithmeticException("-Infinity - -Infinity is undefined")
    } else {
      MinInf
    }
  }

  def div(v: Infinitable, rm: RoundingMode): Infinitable = this * v

  def *(v: Infinitable): Infinitable = {
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

  def unary_-(): PlusInf.type = PlusInf

  def <=(i: BigInt) = true

  def <(i: BigInt) = true

  def >=(i: BigInt) = false

  def >(i: BigInt) = false

  override def toString = "-∞"

  def abs: PlusInf.type = PlusInf

  def finite = throw new ArithmeticException(s"${this} is not finite")
}

case object PlusInf extends Infinitable {
  def +(v: Infinitable): PlusInf.type = PlusInf

  def -(v: Infinitable): PlusInf.type = {
    if (v == PlusInf) {
      throw new ArithmeticException("Infinity - Infinity is undefined")
    } else {
      PlusInf
    }
  }

  def *(v: Infinitable): Infinitable = -(MinInf * v)

  def div(v: Infinitable, rm: RoundingMode): Infinitable = -MinInf.div(v, rm)

  def unary_-(): MinInf.type = MinInf

  def divisible(v: Infinitable) = false

  def <=(i: BigInt) = false

  def <(i: BigInt) = false

  def >=(i: BigInt) = true

  def >(i: BigInt) = true

  override def toString = "+∞"

  def abs: PlusInf.type = PlusInf

  def pow(b: Infinitable): PlusInf.type = {
    b match {
      case MinInf => throw new ArithmeticException(s"pow($this, $b) is undefined")
      case _ => PlusInf
    }
  }

  def finite = throw new ArithmeticException(s"${this} is not finite")
}

case class Finite(i: BigInt) extends AnyVal with Infinitable {
  def finite: BigInt = i

  def +(v: Infinitable): Infinitable = v match {
    case Finite(j) => Finite(i + j)
    case u => u + this
  }

  def -(v: Infinitable): Infinitable = -v + this

  def *(v: Infinitable): Infinitable = v match {
    case Finite(j) => Finite(i * j)
    case u => u * this
  }

  def div(v: Infinitable, rm: RoundingMode): Infinitable = v match {
    case Finite(j) => Finite(Math.divide(i, j, rm))
    case _ => Finite(0)
  }

  def divisible(v: Infinitable): Boolean = v match {
    case Finite(j) => i % j == 0
    case _ => false
  }

  override def unary_-() = Finite(-i)

  def <=(j: BigInt): Boolean = i <= j

  def <(j: BigInt): Boolean = i < j

  def >=(j: BigInt): Boolean = i >= j

  def >(j: BigInt): Boolean = i > j

  override def toString: String = i.toString

  def abs: Infinitable = Finite(i.abs)

  def pow(b: Infinitable): Infinitable = {
    b match {
      case Finite(b) if b.isValidInt => Finite(i.pow(b.intValue))
      case PlusInf => PlusInf
      case MinInf => Finite(0)
      case _ => throw new ArithmeticException(s"Cannot compute $this^$b")
    }
  }

}
