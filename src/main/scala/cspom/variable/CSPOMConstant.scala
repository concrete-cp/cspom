package cspom.variable

import scala.collection.mutable.WeakHashMap

trait CSPOMConstant extends CSPOMExpression {
  def flattenVariables = Seq()
  final def replaceVar(which: CSPOMVariable, by: CSPOMExpression) = this
}

final class IntConstant private (val value: Int) extends CSPOMConstant with IntExpression {
  override def toString = value.toString
  override def equals(o: Any) = o match {
    case i: IntConstant => i.value == value
    case _ => false
  }
  def cspomType = CSPOMInt
}

object IntConstant {

  val cache = new WeakHashMap[Int, IntConstant]
  def apply(value: Int) =
    cache.getOrElseUpdate(value, new IntConstant(value))

}

final class DoubleConstant private (val value: Double) extends CSPOMConstant {
  override def toString = value.toString
  def cspomType = CSPOMDouble
}

object DoubleConstant {
  val cache = new WeakHashMap[Double, DoubleConstant]
  def apply(value: Double) =
    cache.getOrElseUpdate(value, new DoubleConstant(value))

}

object CSPOMTrue extends CSPOMConstant with CSPOMType with BoolExpression {
  override def toString = "true"
  def cspomType = this
  def generalizes(other: CSPOMType) = other == CSPOMTrue
}

object CSPOMFalse extends CSPOMConstant with CSPOMType with BoolExpression {
  override def toString = "false"
  def cspomType = this
  def generalizes(other: CSPOMType) = other == CSPOMFalse
}