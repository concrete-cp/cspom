package cspom.compiler;

import java.util.Collection;
import java.util.Set;

import cspom.constraint.CSPOMConstraint;
import cspom.variable.CSPOMVariable;

public final class CliqueDetector {

    private CliqueDetector() {
    }

    public static boolean haveSubsumingConstraint(
            final CSPOMConstraint constraint,
            final ConstraintSelector validator) {
        for (CSPOMConstraint c : constraint.getScope().get(0).getConstraints()) {
            if (c != constraint && validator.is(c) && subsumes(c, constraint)) {
                return true;
            }
        }
        return false;
    }

    public static boolean subsumes(final CSPOMConstraint allDiff,
            final CSPOMConstraint constraint) {
        return allDiff.involvesAll(constraint.getScope());
    }

    public static boolean allEdges(final CSPOMVariable var,
            final Collection<CSPOMVariable> vars,
            final ConstraintSelector validator) {
        for (CSPOMVariable v : vars) {
            if (!edge(v, var, validator)) {
                return false;
            }
        }
        return true;
    }

    private static boolean edge(final CSPOMVariable var1,
            final CSPOMVariable var2, final ConstraintSelector validator) {
        final Set<CSPOMConstraint> constraints = var1.getConstraints();

        for (CSPOMConstraint c : var2.getConstraints()) {
            if (validator.is(c) && constraints.contains(c)) {
                return true;
            }
        }

        return false;
    }

}
