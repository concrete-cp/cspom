package cspom.variable

final class BoolVariable() extends CSPOMVariable[Boolean]() {

  override def toString = s"bool var"

  def intersected(that: SimpleExpression[_ >: Boolean]): SimpleExpression[Boolean] = {
    if (that.contains(false)) {
      if (that.contains(true)) {
        this
      } else {
        CSPOMConstant(false)
      }
    } else if (that.contains(true)) {
      CSPOMConstant(true)
    } else {
      EmptyVariable
    }
  }

  def contains[S >: Boolean](that: S) = that match {
    case 0 | 1 => true
    case _: Boolean => true
    case _ => false
  }

  def iterator = Iterator(false, true)

  def fullyDefined = true

  def searchSpace = 2

  def isEmpty = false
}

object BoolVariable {
  def apply(): CSPOMVariable[Boolean] = new BoolVariable()
}