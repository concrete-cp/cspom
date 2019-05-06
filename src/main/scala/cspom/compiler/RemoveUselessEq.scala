package cspom.compiler

import cspom.{CSPOM, CSPOMConstraint}
import cspom.compiler.ConstraintCompiler._
import cspom.variable.{CSPOMConstant, CSPOMExpression}

object RemoveUselessEq extends ConstraintCompiler {

  type A = Seq[CSPOMExpression[_]]

  override def functions: CompiledFunctions = Functions('eq)

  override def matchConstraint(c: CSPOMConstraint[_]): Option[Seq[CSPOMExpression[Any]]] = {
    assert(c.function == 'eq)

    val distinct = c.arguments.distinct
    val dsize = distinct.size
    if (dsize < c.arguments.size) {
      Some(distinct)
    } else {
      None
    }

  }

  def compile(c: CSPOMConstraint[_], problem: CSPOM, distinct: A): Delta = {

    if (distinct.size > 1) {
      replaceCtr(c, new CSPOMConstraint(c.result, 'eq, distinct, c.params), problem)
    } else {
      require(c.result != CSPOMConstant(false))
      val d = replaceCtr(c, Nil, problem)
      d ++ replace(c.result, CSPOMConstant(true), problem)
    }

  }

  def selfPropagation = false


}
