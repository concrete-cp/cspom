package cspom.compiler

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.variable.FreeVariable

trait Types extends ConstraintCompilerNoData {

  def matchBool(constraint: CSPOMConstraint[_], problem: CSPOM): Boolean = {
    _types.contains(constraint.function) &&
      constraint.fullScope.iterator.flatMap(_.flatten).exists(_.isInstanceOf[FreeVariable])
  }

  def compile(constraint: CSPOMConstraint[_], problem: CSPOM) =
    _types(constraint.function)(constraint, problem)

  private val _types = types

  def types: Map[Symbol, Function2[CSPOMConstraint[_], CSPOM, Delta]]

  def selfPropagation: Boolean = true
}