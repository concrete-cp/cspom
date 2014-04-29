package cspom.compiler

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.variable.BoolVariable
import cspom.variable.CSPOMConstant
import cspom.variable.FreeVariable

object RemoveAnd extends ConstraintCompilerNoData {
  def matchBool(constraint: CSPOMConstraint[_], problem: CSPOM) = constraint match {
    case CSPOMConstraint(CSPOMConstant(true), 'and, _, _) => true
    case _ => false
  }

  def compile(constraint: CSPOMConstraint[_], problem: CSPOM) = {

    require(constraint.arguments.forall(v => v.isInstanceOf[FreeVariable] || v.isInstanceOf[BoolVariable] || v.isTrue), constraint)

    problem.removeConstraint(constraint)

    Delta().removed(constraint) ++ replace(constraint.arguments.distinct, CSPOMConstant(true), problem)

  }

  def selfPropagation = false
}
