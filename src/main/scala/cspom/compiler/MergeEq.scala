package cspom.compiler

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMVariable
import cspom.variable.SimpleExpression

/**
 * If given constraint is an all-equal constraint, merges and removes all
 * auxiliary variables.
 */
object MergeEq extends ConstraintCompiler {

  type A = Seq[SimpleExpression[Any]]

  override def mtch(c: CSPOMConstraint[_], p: CSPOM) = c match {
    case CSPOMConstraint(CSPOMConstant(true), 'eq, args: Seq[_], params) if !params.contains("neg") &&
      params.get("offset").forall(_ == 0) && args.forall(_.isInstanceOf[SimpleExpression[_]]) =>
      val se = args.asInstanceOf[Seq[SimpleExpression[_]]].distinct

      if (se.tail.nonEmpty) {
        Some(se)
      } else {
        None
      }

    case _ => None
  }

  def compile(constraint: CSPOMConstraint[_], problem: CSPOM, se: A) = {

    problem.removeConstraint(constraint)

    val delta = Delta().removed(constraint)

    val merged = se.reduceLeft(_ intersected _)

    /**
     * Update the constraints of the problem
     */
    delta ++ replace(se, merged, problem)

  }

  def selfPropagation = true

}
