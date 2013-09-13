package cspom.compiler

import cspom.constraint.CSPOMConstraint
import cspom.constraint.FunctionalConstraint
import cspom.constraint.GeneralConstraint
import cspom.extension.ExtensionConstraint

trait ConstraintCompiler {
  def compile(constraint: CSPOMConstraint) = {
    constraint match {
      case c: GeneralConstraint => compileGeneral(c)
      case c: FunctionalConstraint => compileFunctional(c)
      case c: ExtensionConstraint => compileExtension(c)
    }
  }
  
  def compileGeneral(constraint: GeneralConstraint) = false
  
  def compileFunctional(constraint: FunctionalConstraint) = false
  
  def compileExtension(constraint: ExtensionConstraint) = false
}
