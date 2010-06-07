package cspom.compiler.patterns;

import cspom.CSPOM;
import cspom.constraint.CSPOMConstraint;
import cspom.constraint.GeneralConstraint;
import cspom.variable.BooleanDomain;
import cspom.variable.CSPOMVariable;

public final class RemoveAnd implements ConstraintCompiler {

    private final CSPOM problem;

    public RemoveAnd(final CSPOM problem) {
        this.problem = problem;
    }

    @Override
    public void compile(final CSPOMConstraint constraint) {
        if ("and".equals(constraint.getDescription())
                && constraint instanceof GeneralConstraint) {
            for (CSPOMVariable v : constraint) {
                v.setDomain(BooleanDomain.TRUE);
            }
            problem.removeConstraint(constraint);
        }
    }

}
