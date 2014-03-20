package cspom.compiler

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.variable.BoolVariable
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMExpression

object SplitEqVec extends ConstraintCompiler {

  type A = (CSPOMSeq[CSPOMExpression[Any]], CSPOMSeq[CSPOMExpression[Any]])

  override def constraintMatcher: PartialFunction[CSPOMConstraint[_], A] = {
    case CSPOMConstraint(CSPOMConstant(true), 'eq, Seq(a: CSPOMSeq[CSPOMExpression[Any]], b: CSPOMSeq[CSPOMExpression[Any]]), p) if (a.size == b.size) &&
      !p.contains("neg") && !p.contains("offset") => (a, b)
  }

  def compile(constraint: CSPOMConstraint[_], problem: CSPOM, data: A) = {

    val (a, b) = data
    val newConstraints = (a zip b).map { case (a, b) => CSPOMConstraint('eq, Seq(a, b)) }

    replaceCtr(constraint, newConstraints, problem)

  }

  def selfPropagation = false
}
