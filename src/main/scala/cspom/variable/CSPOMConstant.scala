package cspom.variable

import scala.collection.mutable.WeakHashMap

final class IntConstant private (val value: Int) extends CSPOMConstant with IntExpression {
  override def toString = value.toString
  override def equals(o: Any) = o match {
    case i: IntConstant => i.value == value
    case _ => false
  }
}

object IntConstant {

  val cache = new WeakHashMap[Int, IntConstant]
  def apply(value: Int) =
    cache.getOrElseUpdate(value, new IntConstant(value))

  def unapply(c: IntConstant): Option[Int] = Some(c.value)
}

final class DoubleConstant private (val value: Double) extends CSPOMConstant {
  override def toString = value.toString
}

object DoubleConstant {
  val cache = new WeakHashMap[Double, DoubleConstant]
  def apply(value: Double) =
    cache.getOrElseUpdate(value, new DoubleConstant(value))

}

object CSPOMTrue extends CSPOMConstant with BoolExpression {
  override def toString = "true"
  def cspomType = this
  def neg = CSPOMFalse
}

object CSPOMFalse extends CSPOMConstant with BoolExpression {
  override def toString = "false"
  def cspomType = this
  def neg = CSPOMTrue
}