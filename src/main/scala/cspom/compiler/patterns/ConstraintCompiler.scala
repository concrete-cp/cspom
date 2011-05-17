package cspom.compiler.patterns;

import cspom.constraint.CSPOMConstraint;

trait ConstraintCompiler {
  def compile(constraint: CSPOMConstraint): Unit;
}
