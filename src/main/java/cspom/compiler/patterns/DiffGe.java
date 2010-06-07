package cspom.compiler.patterns;

import java.util.List;

import cspom.CSPOM;
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
        for (CSPOMConstraint c : variable.getConstraints()) {
            if ("ge".equals(c.getDescription())) {
                if (c instanceof FunctionalConstraint) {
                    final FunctionalConstraint geConstraint = (FunctionalConstraint) c;
                    final CSPOMVariable[] scope = geConstraint.getArguments();
                    if (scope.length == 2 && scope[0] == variable) {
                        return geConstraint;
                    }
                } else {
                    final List<CSPOMVariable> scope = c.getScope();
                    if (scope.size() == 2 && scope.get(0) == variable) {
                        return c;
                    }
                }
            }
        }
        return null;
    }

}
