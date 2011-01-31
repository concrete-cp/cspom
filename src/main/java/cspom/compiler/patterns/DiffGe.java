package cspom.compiler.patterns;

import java.util.List;
import java.util.NoSuchElementException;

import com.google.common.collect.Iterables;

import cspom.CSPOM;
import cspom.constraint.AbstractConstraint;
import cspom.constraint.CSPOMConstraint;
import cspom.constraint.FunctionalConstraint;
import cspom.constraint.GeneralConstraint;
import cspom.variable.CSPOMVariable;

/**
 * If constraint is the sub() constraint, converts a=sub(y,z), x=abs(a) to
 * x=absdiff(y,z). No other constraint may imply the auxiliary constraint a.
 */
public final class DiffGe implements ConstraintCompiler {
    private final CSPOM problem;

    public DiffGe(final CSPOM problem) {
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
        final CSPOMConstraint geConstraint = geConstraint(result);
        if (geConstraint == null) {
            return;
        }
        problem.removeConstraint(subConstraint);
        problem.removeConstraint(geConstraint);

        if (geConstraint instanceof FunctionalConstraint) {
            final CSPOMVariable[] scope = new CSPOMVariable[] {
                    subConstraint.getVariable(1), subConstraint.getVariable(2),
                    geConstraint.getVariable(2) };
            problem.addConstraint(new FunctionalConstraint(geConstraint
                    .getVariable(0), "diffGe", null, scope));
        } else {
            final CSPOMVariable[] scope = new CSPOMVariable[] {
                    subConstraint.getVariable(1), subConstraint.getVariable(2),
                    geConstraint.getVariable(1) };
            problem.addConstraint(new GeneralConstraint("diffGe", null, scope));
        }
    }

    private static CSPOMConstraint geConstraint(final CSPOMVariable variable) {
        final CSPOMConstraint geConstraint;
        try {
            geConstraint = Iterables.find(variable.getConstraints(),
                    AbstractConstraint.matchesDescription("ge"));
        } catch (NoSuchElementException e) {
            return null;
        }
        final List<CSPOMVariable> scope;
        if (geConstraint instanceof FunctionalConstraint) {
            scope = ((FunctionalConstraint) geConstraint).getArguments();
        } else {
            scope = geConstraint.getScope();
        }
        if (scope.size() == 2 && scope.get(0) == variable) {
            return geConstraint;
        }
        return null;
    }

}
