package cspom.compiler

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.variable.CSPOMExpression

trait Types extends ConstraintCompiler {
  type A = Map[CSPOMExpression[_], CSPOMExpression[_]]

  def types: PartialFunction[CSPOMConstraint[_], A]

  override def mtch(constraint: CSPOMConstraint[_], problem: CSPOM) =
    types
      .lift(constraint)
      .map(m => m.filter { case (k, v) => k ne v })
      .filter(_.nonEmpty)

  def selfPropagation: Boolean = true

  def compile(c: CSPOMConstraint[_], p: CSPOM, d: A) = {
    d.map { case (k, v) => replace(k, v, p) }.reduce(_ ++ _)
  }
}