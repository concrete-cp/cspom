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

class FreeVariable extends CSPOMVariable[Any] {
  override def toString = s"free var"
  def intersected(other: SimpleExpression[_ >: Any]) = other
  def contains[S >: Any](that: S) = true
  def iterator = throw new UnsupportedOperationException
  def fullyDefined = false
  def searchSpace = Double.PositiveInfinity
}



