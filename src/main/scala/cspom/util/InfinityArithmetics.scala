package cspom.util

import com.google.common.math.IntMath
import java.math.RoundingMode

sealed trait Value extends Ordered[Value] {
  def +(v: Value): Value
  def -(v: Value): Value = this + (-v)
  def *(v: Value): Value
  def div(v: Value, rm: RoundingMode): Value
  def divisible(v: Value): Boolean
  def unary_-(): Value
}

case object MinInf extends Value {
  def +(v: Value) = MinInf
  def *(v: Value) = {
    if (v > Finite(0)) {
      MinInf
    } else if (v < Finite(0)) {
      PlusInf
    } else {
      throw new UnsupportedOperationException
    }
  }
  def div(v: Value, rm: RoundingMode) = this * v
  def divisible(v: Value) = false
  def compare(v: Value) = -1
  def unary_-() = PlusInf
}
case object PlusInf extends Value {
  def +(v: Value) = PlusInf
  def *(v: Value) = -(MinInf * v)
  def div(v: Value, rm: RoundingMode) = -(MinInf.div(v, rm))
  def compare(v: Value) = 1
  def unary_-() = MinInf
  def divisible(v: Value) = false
}
case class Finite(i: Int) extends Value {
  def +(v: Value) = v match {
    case Finite(j) => Finite(IntMath.checkedAdd(i, j))
    case u => u + this
  }
  def *(v: Value) = v match {
    case Finite(j) => Finite(IntMath.checkedMultiply(i, j))
    case u => u * this
  }
  def div(v: Value, rm: RoundingMode) = v match {
    case Finite(j) => Finite(IntMath.divide(i, j, rm))
    case u => Finite(0)
  }
  def divisible(v: Value) = v match {
    case Finite(j) => i % j == 0
    case u => false
  }
  def unary_-() = Finite(-i)
  def compare(v: Value) = v match {
    case Finite(j) => i - j
    case u => -u.compare(v)
  }
}