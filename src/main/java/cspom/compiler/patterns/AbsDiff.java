package cspom.compiler.patterns;

import java.util.NoSuchElementException;

import com.google.common.collect.Iterables;

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
        if (!("sub".equals(constraint.description()) && constraint instanceof FunctionalConstraint)) {
            return;
        }
        final FunctionalConstraint subConstraint = (FunctionalConstraint) constraint;
        final CSPOMVariable result = subConstraint.resultVariable();
        if (!result.auxiliary() || result.constraints().size() != 2) {
            return;
        }
        final FunctionalConstraint absConstraint = absConstraint(result);
        if (absConstraint == null) {
            return;
        }
        problem.removeConstraint(subConstraint);
        problem.removeConstraint(absConstraint);
        problem.addConstraint(new FunctionalConstraint(absConstraint
                .resultVariable(), "absdiff", null, subConstraint
                .arguments()));
    }

    private static FunctionalConstraint absConstraint(
            final CSPOMVariable<?> variable) {
        
        final FunctionalConstraint fConstraint;
        try {
            fConstraint = (FunctionalConstraint) Iterables.find(
                    variable.getConstraints(),
                    FunctionalConstraint.matchesDescription("abs"));
        } catch (NoSuchElementException e) {
            return null;
        }
        if (Iterables.getOnlyElement(fConstraint.arguments()) == variable) {
            return fConstraint;
        }
        return null;
    }

}
