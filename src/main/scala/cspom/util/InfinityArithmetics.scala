package cspom.util

import com.google.common.math.IntMath
import java.math.RoundingMode

sealed trait Infinitable extends Ordered[Infinitable] {
  def +(v: Infinitable): Infinitable
  def -(v: Infinitable): Infinitable = this + (-v)
  def *(v: Infinitable): Infinitable
  def div(v: Infinitable, rm: RoundingMode): Infinitable
  def divisible(v: Infinitable): Boolean
  def unary_-(): Infinitable
  def isInfinity = (this eq MinInf) || (this eq PlusInf)

  def <=(i: Int): Boolean
  def <(i: Int): Boolean

}

case object MinInf extends Infinitable {
  def +(v: Infinitable) = MinInf
  def *(v: Infinitable) = {
    if (v > Finite(0)) {
      MinInf
    } else if (v < Finite(0)) {
      PlusInf
    } else {
      throw new UnsupportedOperationException
    }
  }
  def div(v: Infinitable, rm: RoundingMode) = this * v
  def divisible(v: Infinitable) = false
  def compare(v: Infinitable) = -1
  def unary_-() = PlusInf
  def <=(i: Int) = true
  def <(i: Int) = true
}
case object PlusInf extends Infinitable {
  def +(v: Infinitable) = PlusInf
  def *(v: Infinitable) = -(MinInf * v)
  def div(v: Infinitable, rm: RoundingMode) = -(MinInf.div(v, rm))
  def compare(v: Infinitable) = 1
  def unary_-() = MinInf
  def divisible(v: Infinitable) = false
  def <=(i: Int) = false
  def <(i: Int) = false
}
case class Finite(i: Int) extends Infinitable {
  def +(v: Infinitable) = v match {
    case Finite(j) => Finite(IntMath.checkedAdd(i, j))
    case u => u + this
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
  def compare(v: Infinitable) = v match {
    case Finite(j) => i - j
    case u => -u.compare(v)
  }
  def <=(j: Int) = i <= j
  def <(j: Int) = i < j
}