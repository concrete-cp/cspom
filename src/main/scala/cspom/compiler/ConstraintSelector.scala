package cspom.compiler;

import cspom.CSPOMConstraint

trait ConstraintSelector {
    def is(constraint: CSPOMConstraint): Boolean;
}