package cspom.constraint

import cspom.variable.CSPOMVariable;
trait PermutableConstraint extends CSPOMConstraint {
  def standardize(scope: CSPOMVariable[_]*): PermutableConstraint
}