package cspom.variable;

import cspom.CSPOMConstraint
import cspom.CSPOM
import scala.collection.mutable.HashMap

/**
 * This class defines and implements CSP variables.
 *
 * @author vion
 *
 */

class FreeVariable(params: Set[Any]) extends CSPOMVariable[Any](params) {
  def this(params: Any*) = this(params.toSet)
  override def toString = s"free variable"
  def intersected(other: SimpleExpression[_ >: Any]) = other
  def contains[S >: Any](that: S) = true
}



