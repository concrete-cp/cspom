package cspom.compiler.patterns;

import java.util.Deque;

import cspom.CSPOM;
import cspom.constraint.CSPOMConstraint;
import cspom.constraint.FunctionalConstraint;
import cspom.constraint.GeneralConstraint;
import cspom.variable.BooleanDomain;

public final class DeReify implements ConstraintCompiler {

    private final CSPOM problem;
    private final Deque<CSPOMConstraint> constraints;

    public DeReify(final CSPOM problem, final Deque<CSPOMConstraint> constraints) {
        this.problem = problem;
        this.constraints = constraints;
    }

    @Override
    public void compile(final CSPOMConstraint c) {
        if (c instanceof FunctionalConstraint) {
            final FunctionalConstraint fc = (FunctionalConstraint) c;
            if (BooleanDomain.TRUE.equals(fc.getResultVariable().getDomain())) {
                deReify(fc);
            }
        }
    }

    private void deReify(final FunctionalConstraint constraint) {
        problem.removeConstraint(constraint);
        final CSPOMConstraint newConstraint = new GeneralConstraint(
                constraint.getDescription(), constraint.getParameters(),
                constraint.getArguments());
        problem.addConstraint(newConstraint);
        constraints.add(newConstraint);
    }

}
