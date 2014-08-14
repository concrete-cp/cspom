package cspom.variable

import cspom.CSPOM

final class BoolVariable(params: Map[String, Any] = Map())
  extends CSPOMVariable[Boolean](params) {

  override def toString = s"boolean variable$displayParams"

  def intersected(that: SimpleExpression[_ >: Boolean]): SimpleExpression[Boolean] = that match {
    case t: BoolVariable => new BoolVariable(t.params ++ params)
    case c @ CSPOMConstant(t: Boolean) => CSPOMConstant(t, c.params ++ params)
    case _ => throw new IllegalArgumentException
  }

  def contains[S >: Boolean](that: S) = {
    that.isInstanceOf[Boolean]
  }

  def iterator = Iterator(false, true)

  def fullyDefined = true
  def searchSpace = 2
}

object BoolVariable {
  def boolExpression(e: SimpleExpression[_]): SimpleExpression[Boolean] = e match {
    case f: FreeVariable => new BoolVariable(f.params)
    case b: BoolVariable => b
    case c @ CSPOMConstant(_: Boolean) => c.asInstanceOf[CSPOMConstant[Boolean]]
  }

}