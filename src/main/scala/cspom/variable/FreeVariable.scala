package cspom.variable;

import cspom.CSPOMConstraint
import cspom.CSPOM
import scala.collection.mutable.HashMap

class FreeVariable extends CSPOMVariable[Any] {
  override def toString = s"free var"
  def intersected(other: SimpleExpression[_ >: Any]) = other
  def contains[S >: Any](that: S) = true
  def iterator = throw new UnsupportedOperationException
  def fullyDefined = false
  def searchSpace = Double.PositiveInfinity
  def isEmpty = false
}

object FreeVariable {
  def apply(): CSPOMVariable[Any] = new FreeVariable()
}

object EmptyVariable extends CSPOMVariable[Nothing] {
  override def toString = s"empty var"
  def intersected(other: SimpleExpression[_]) = this
  def contains[S](that: S) = false
  def fullyDefined = true
  def iterator = Iterator.empty
  def searchSpace = 0
  def isEmpty = true
}