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

  def domain = Seq(false, true)

}