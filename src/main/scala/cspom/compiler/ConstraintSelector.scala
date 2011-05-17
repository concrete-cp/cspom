package cspom.compiler;

import cspom.constraint.CSPOMConstraint

trait ConstraintSelector {
    def is(constraint: CSPOMConstraint): Boolean;
}