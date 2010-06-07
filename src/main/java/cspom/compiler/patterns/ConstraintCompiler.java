package cspom.compiler.patterns;

import cspom.constraint.CSPOMConstraint;

public interface ConstraintCompiler {
    void compile(final CSPOMConstraint constraint);
}
