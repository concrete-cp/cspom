package cspom.variable

import cspom.CSPOM

final class BoolVariable() extends CSPOMVariable[Boolean]() {

  override def toString = s"bool var"

  def intersected(that: SimpleExpression[_ >: Boolean]): SimpleExpression[Boolean] = that match {
    case _: BoolVariable => this
    case c: CSPOMConstant[_] =>
      require(c.value.isInstanceOf[Boolean]); c.asInstanceOf[CSPOMConstant[Boolean]]
    case _ => EmptyVariable
  }

  def contains[S >: Boolean](that: S) = {
    that.isInstanceOf[Boolean]
  }

  def iterator = Iterator(false, true)

  def fullyDefined = true
  def searchSpace = 2
  def isEmpty = false
}

object BoolVariable {
  def apply(): CSPOMVariable[Boolean] = new BoolVariable()
}