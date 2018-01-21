package cspom.compiler

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.variable.CSPOMConstant
import cspom.variable.SimpleExpression
import com.typesafe.scalalogging.LazyLogging
import cspom.variable.BoolExpression
import ConstraintCompiler._
/**
 * If given constraint is an all-equal constraint, merges and removes all
 * auxiliary variables.
 */
object MergeEq extends ConstraintCompilerNoData with LazyLogging {

  override def matchBool(c: CSPOMConstraint[_], p: CSPOM): Boolean = {
    c.function == 'eq && c.arguments.forall(_.isInstanceOf[SimpleExpression[_]])
  }

  def compile(constraint: CSPOMConstraint[_], problem: CSPOM): Delta = {
    val se = constraint.arguments.map(_.asInstanceOf[SimpleExpression[_]])

    val merged = se.reduceLeft(_ intersected _)

    val result = BoolExpression.coerce(constraint.result)

    if (merged.isEmpty) {
      removeCtr(constraint, problem) ++
        replace(constraint.result, result intersected CSPOMConstant(false), problem)
    } else if (se.forall(_.searchSpace == 1)) {
      removeCtr(constraint, problem) ++
        replace(constraint.result, result intersected CSPOMConstant(true), problem)
    } else if (result.isTrue) {
      val delta = removeCtr(constraint, problem) ++
        se.map(replace(_, merged, problem)).reduce(_ ++ _)
      //println(se + " : " + delta)
      delta
    } else {
      replace(constraint.result, result, problem)
    }
  }

  def selfPropagation = true

}
