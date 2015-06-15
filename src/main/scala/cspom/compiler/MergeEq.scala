package cspom.compiler

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMVariable
import cspom.variable.SimpleExpression
import com.typesafe.scalalogging.LazyLogging
import cspom.variable.BoolExpression

/**
 * If given constraint is an all-equal constraint, merges and removes all
 * auxiliary variables.
 */
object MergeEq extends ConstraintCompilerNoData with LazyLogging {

  override def matchBool(c: CSPOMConstraint[_], p: CSPOM) =
    c.function == 'eq && c.arguments.forall(_.isInstanceOf[SimpleExpression[_]])

  def compile(constraint: CSPOMConstraint[_], problem: CSPOM) = {
    require(!constraint.params.contains("neg") && !constraint.params.contains("offset"), "neg and offset parameters are deprecated for the eq constraint")

    val se = constraint.arguments.map(_.asInstanceOf[SimpleExpression[_]])

    val merged = se.reduceLeft(_ intersected _)

    if (merged.isEmpty) {
      removeCtr(constraint, problem) ++
        replace(constraint.result, CSPOMConstant(false), problem)
    } else if (se.forall(_ == merged)) {
      removeCtr(constraint, problem) ++
        replace(constraint.result, CSPOMConstant(true), problem)
    } else if (constraint.result.isTrue) {
      removeCtr(constraint, problem) ++
        se.map(replace(_, merged, problem)).reduce(_ ++ _)
    } else {
      replace(constraint.result, BoolExpression.coerce(constraint.result), problem)
    }
  }

  def selfPropagation = true

}
