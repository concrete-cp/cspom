package cspom.compiler

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.variable.BoolVariable
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMExpression

object SplitEqVec extends ConstraintCompiler {

  type A = Seq[(CSPOMExpression[_], CSPOMExpression[_])]

  override def constraintMatcher: PartialFunction[CSPOMConstraint[_], A] = {
    case CSPOMConstraint(CSPOMConstant(true), 'eq, Seq(a: CSPOMSeq[_], b: CSPOMSeq[_]), p) if (a.size == b.size) &&
      !p.contains("neg") && !p.contains("offset") => (a, b).zipped.toSeq
  }

  def compile(constraint: CSPOMConstraint[_], problem: CSPOM, data: A) = {

    val newConstraints = data.map { case (a, b) => CSPOMConstraint('eq)(a, b) }

    replaceCtr(constraint, newConstraints, problem)

  }

  def selfPropagation = false
}
