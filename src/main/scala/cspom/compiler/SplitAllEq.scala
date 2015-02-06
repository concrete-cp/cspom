package cspom.compiler

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.variable.CSPOMConstant
import cspom.variable.BoolVariable

object SplitAllEq extends ConstraintCompilerNoData {
  def matchBool(constraint: CSPOMConstraint[_], problem: CSPOM) =
    constraint.function == 'eq && constraint.arguments.size > 2

  def compile(constraint: CSPOMConstraint[_], problem: CSPOM) = {
    val rs = (for (Seq(a, b) <- constraint.arguments.sliding(2)) yield {
      CSPOMConstraint(new BoolVariable, 'eq, Seq(a, b))
    }).toList

    val and = CSPOMConstraint(constraint.result, 'and, rs.map(_.result))

    replaceCtr(constraint, and :: rs, problem)
  }

  def selfPropagation = false
}
