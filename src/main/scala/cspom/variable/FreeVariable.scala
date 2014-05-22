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

class FreeVariable(params: Map[String, Any]) extends CSPOMVariable[Any](params) {
  override def toString = s"free variable$displayParams"
  def intersected(other: SimpleExpression[_ >: Any]) = other
  def contains[S >: Any](that: S) = true
  def domainValues = throw new UnsupportedOperationException
  def fullyDefined = false
}



