package cspom.compiler

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMTrue
import cspom.variable.CSPOMVariable
import cspom.variable.SimpleExpression

/**
 * If given constraint is an all-equal constraint, merges and removes all
 * auxiliary variables.
 */
object MergeEq extends ConstraintCompilerNoData {

  override def matchBool(c: CSPOMConstraint[_], p: CSPOM) = c match {
    case CSPOMConstraint(CSPOMTrue, 'eq, args: Seq[SimpleExpression[Any]], params) if !params.contains("neg") &&
      params.get("offset").forall(_ == 0) && args.forall(_.isInstanceOf[SimpleExpression[_]]) =>

      true

    case _ => false

  }

  def compile(constraint: CSPOMConstraint[_], problem: CSPOM) = {
    problem.removeConstraint(constraint)

    val delta = Delta().removed(constraint)

    val se = constraint.arguments.map {
      case s: SimpleExpression[_] => s
      case _ => throw new IllegalStateException
    }

    val merged = se.reduceLeft(_ intersected _)

    val names = problem.namedExpressions.filter {
      case (n, v) => se.contains(v)
    } map (_._1)

    for (v <- se; n <- names) {
      problem.replaceExpression(n, v)
    }

    /**
     * Update the constraints of the problem
     */
    delta ++ replace(se, merged, problem)

  }

  def selfPropagation = true

}
