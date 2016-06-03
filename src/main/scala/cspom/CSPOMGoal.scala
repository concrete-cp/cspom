package cspom

import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMVariable

sealed trait CSPOMGoal[+T] {
  def expr: Option[CSPOMExpression[T]]
}

object CSPOMGoal {
  case object Satisfy extends CSPOMGoal[Nothing] {
    def expr = None
  }

  case class Maximize[T: Ordering](e: CSPOMExpression[T]) extends CSPOMGoal[T] {
    def expr = Some(e)
  }

  case class Minimize[T: Ordering](e: CSPOMExpression[T]) extends CSPOMGoal[T] {
    def expr = Some(e)
  }
}