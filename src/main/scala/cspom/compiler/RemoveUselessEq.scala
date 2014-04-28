package cspom.compiler

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMVariable
import cspom.variable.BoolVariable
import cspom.variable.CSPOMExpression

object RemoveUselessEq extends ConstraintCompiler {

  type A = Seq[CSPOMExpression[_]]

  override def matchConstraint(c: CSPOMConstraint[_]) = {
    val distinct = c.arguments.distinct
    val dsize = distinct.size
    if (dsize <= 1 || distinct.size < c.arguments.size) {
      Some(distinct)
    } else {
      None
    }
  }

  def compile(c: CSPOMConstraint[_], problem: CSPOM, distinct: A): Delta = {

    if (distinct.size > 1) {
      replaceCtr(c, new CSPOMConstraint(c.result, 'eq, distinct, c.params), problem)
    } else {
      replaceCtr(c, Seq(), problem)
    }

  }

  def selfPropagation = false
}
