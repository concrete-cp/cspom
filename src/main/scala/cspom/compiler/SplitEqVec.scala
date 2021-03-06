package cspom.compiler

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMExpression

object SplitEqVec extends ConstraintCompiler {

  type A = Seq[(CSPOMExpression[_], CSPOMExpression[_])]

  def functions = Functions("eq")

  override def constraintMatcher: PartialFunction[CSPOMConstraint[_], A] = {
    case CSPOMConstraint(CSPOMConstant(true), "eq", Seq(a: CSPOMSeq[_], b: CSPOMSeq[_]), p) if a.size == b.size =>
      require(!p.contains("neg") && !p.contains("offset"))

      (a, b).zipped.toSeq
  }

  def compile(constraint: CSPOMConstraint[_], problem: CSPOM, data: A): Delta = {

    val newConstraints = data.map { case (a, b) => CSPOMConstraint("eq")(a, b) }

    ConstraintCompiler.replaceCtr(constraint, newConstraints, problem)

  }

  def selfPropagation = false
}
