package cspom.compiler.patterns;

import java.util.Deque;

import cspom.CSPOM;
import cspom.constraint.CSPOMConstraint;
import cspom.constraint.FunctionalConstraint;
import cspom.constraint.GeneralConstraint;
import cspom.variable.TrueDomain$;

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
            if (TrueDomain$.MODULE$ == fc.result().domain()) {
                deReify(fc);
            }
        }
    }

    private void deReify(final FunctionalConstraint constraint) {
        problem.removeConstraint(constraint);
        final CSPOMConstraint newConstraint = new GeneralConstraint(
                constraint.description(), constraint.parameters(),
                constraint.arguments());
        problem.addConstraint(newConstraint);
        constraints.add(newConstraint);
    }

}
