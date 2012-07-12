package cspom.compiler.patterns;

import cspom.constraint.CSPOMConstraint
import cspom.constraint.FunctionalConstraint
import cspom.constraint.GeneralConstraint
import cspom.extension.ExtensionConstraint

trait ConstraintCompiler {
  def compile(constraint: CSPOMConstraint) {
    constraint match {
      case c: GeneralConstraint => compileGeneral(c)
      case c: FunctionalConstraint => compileFunctional(c)
      case c: ExtensionConstraint => compileExtension(c)
    }
  }
  
  def compileGeneral(constraint: GeneralConstraint) {}
  
  def compileFunctional(constraint: FunctionalConstraint) {}
  
  def compileExtension(constraint: ExtensionConstraint) {}
}
