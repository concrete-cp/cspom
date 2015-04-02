package cspom.compiler

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMVariable
import cspom.variable.SimpleExpression
import com.typesafe.scalalogging.LazyLogging

/**
 * If given constraint is an all-equal constraint, merges and removes all
 * auxiliary variables.
 */
object MergeEq extends ConstraintCompilerNoData with LazyLogging {

  override def matchBool(c: CSPOMConstraint[_], p: CSPOM) = c match {
    case CSPOMConstraint(CSPOMConstant(true), 'eq, args: Seq[_], params) if params.get("neg").forall(_ == false) &&
      params.get("offset").forall(_ == 0) && args.forall(_.isInstanceOf[SimpleExpression[_]]) =>
      true

    case _ => false
  }

  def compile(constraint: CSPOMConstraint[_], problem: CSPOM) = {
    val se = constraint.arguments.map(_.asInstanceOf[SimpleExpression[_]])
    val merged = se.reduceLeft(_ intersected _)

    //   println(s"Merging $se to $merged")

    //val oldConstraints = se.flatMap(problem.deepConstraints(_)).map(_.toString).sorted

    /**
     * Update the constraints of the problem
     */
    val d = removeCtr(constraint, problem) ++ se.map(replace(_, merged, problem)).reduce(_ ++ _)

    //println(problem)

    d
  }

  def selfPropagation = true

}
