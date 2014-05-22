package cspom.util

sealed trait Value extends Ordered[Value] {
  def +(v: Value): Value
  def -(v: Value): Value = this + (-v)
  def *(v: Value): Value
  def /(v: Value): Value
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
  def /(v: Value) = this * v
  def compare(v: Value) = -1
  def unary_-() = PlusInf
}
case object PlusInf extends Value {
  def +(v: Value) = PlusInf
  def *(v: Value) = -(MinInf * v)
  def /(v: Value) = -(MinInf / v)
  def compare(v: Value) = 1
  def unary_-() = MinInf
}
case class Finite(i: Int) extends Value {
  def +(v: Value) = v match {
    case Finite(j) => Finite(i + j)
    case u => u + this
  }
  def *(v: Value) = v match {
    case Finite(j) => Finite(i * j)
    case u => u * this
  }
  def /(v: Value) = v match {
    case Finite(0) => throw new UnsupportedOperationException
    case Finite(j) => Finite(i / j)
    case u => Finite(0)
  }
  def unary_-() = Finite(-i)
  def compare(v: Value) = v match {
    case Finite(j) => i - j
    case u => -u.compare(v)
  }
}