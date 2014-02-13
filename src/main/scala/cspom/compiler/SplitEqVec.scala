package cspom.compiler

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.variable.BoolVariable
import cspom.variable.CSPOMFalse
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMTrue
import cspom.variable.CSPOMExpression

object SplitEqVec extends ConstraintCompiler {

  type A = (CSPOMSeq[CSPOMExpression], CSPOMSeq[CSPOMExpression])

  override def constraintMatcher: PartialFunction[CSPOMConstraint, A] = {
    case CSPOMConstraint(CSPOMTrue, 'eq, Seq(a: CSPOMSeq[CSPOMExpression], b: CSPOMSeq[CSPOMExpression]), p) if (a.size == b.size) &&
      !p.contains("neg") && !p.contains("offset") => (a, b)
  }

  def compile(constraint: CSPOMConstraint, problem: CSPOM, data: A) = {

    val (a, b) = data
    val newConstraints = (a zip b).map { case (a, b) => new CSPOMConstraint('eq, a, b) }

    replaceCtr(constraint, newConstraints, problem)

  }

  def selfPropagation = false
}
