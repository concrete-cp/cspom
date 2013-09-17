package cspom.compiler

import cspom.CSPOMConstraint
import cspom.variable.CSPOMTrue

trait ConstraintCompiler {
  def compile(constraint: CSPOMConstraint) = {
    if (constraint.function == "extension") {
      compileExtension(constraint)
    } else if (constraint.result == CSPOMTrue) {
      compileGeneral(constraint)
    } else {
      compileFunctional(constraint)
    }
  }

  def compileGeneral(constraint: CSPOMConstraint) = false

  def compileFunctional(constraint: CSPOMConstraint) = false

  def compileExtension(constraint: CSPOMConstraint) = false
}
