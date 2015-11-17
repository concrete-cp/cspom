package cspom

import cspom.variable.CSPOMExpression

sealed trait CSPOMGoal extends Parameterized

object CSPOMGoal {
  case class Satisfy(params: Map[String, Any] = Map())
    extends CSPOMGoal

  case class Maximize[T: Ordering](
    e: CSPOMExpression[T],
    params: Map[String, Any] = Map())
      extends CSPOMGoal

  case class Minimize[T: Ordering](
    e: CSPOMExpression[T],
    params: Map[String, Any] = Map())
      extends CSPOMGoal
}