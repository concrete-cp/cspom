package cspom.compiler;

import cspom.constraint.CSPOMConstraint;

public interface ConstraintSelector {
    boolean is(CSPOMConstraint constraint);
}