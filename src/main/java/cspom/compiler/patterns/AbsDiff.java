package cspom.compiler.patterns;

import cspom.CSPOM;
import cspom.constraint.CSPOMConstraint;
import cspom.constraint.FunctionalConstraint;
import cspom.variable.CSPOMVariable;

/**
 * If constraint is the sub() constraint, converts a=sub(y,z), x=abs(a) to
 * x=absdiff(y,z). No other constraint may imply the auxiliary constraint a.
 */
public final class AbsDiff implements ConstraintCompiler {

    private final CSPOM problem;

    /**
     * If constraint is the sub() constraint, converts a=sub(y,z), x=abs(a) to
     * x=absdiff(y,z). No other constraint may imply the auxiliary constraint a.
     */
    public AbsDiff(final CSPOM problem) {
        this.problem = problem;
    }

    @Override
    public void compile(final CSPOMConstraint constraint) {
        if (!("sub".equals(constraint.getDescription()) && constraint instanceof FunctionalConstraint)) {
            return;
        }
        final FunctionalConstraint subConstraint = (FunctionalConstraint) constraint;
        final CSPOMVariable result = subConstraint.getResultVariable();
        if (!result.isAuxiliary() || result.getConstraints().size() != 2) {
            return;
        }
        final FunctionalConstraint absConstraint = absConstraint(result);
        if (absConstraint == null) {
            return;
        }
        problem.removeConstraint(subConstraint);
        problem.removeConstraint(absConstraint);
        problem.addConstraint(new FunctionalConstraint(absConstraint
                .getResultVariable(), "absdiff", null, subConstraint
                .getArguments()));
    }

    private static FunctionalConstraint absConstraint(
            final CSPOMVariable variable) {
        for (CSPOMConstraint c : variable.getConstraints()) {
            if ("abs".equals(c.getDescription())
                    && c instanceof FunctionalConstraint) {
                final FunctionalConstraint fConstraint = (FunctionalConstraint) c;
                final CSPOMVariable[] arguments = fConstraint.getArguments();
                if (arguments.length == 1 && arguments[0] == variable) {
                    return fConstraint;
                }
            }
        }
        return null;
    }

}
