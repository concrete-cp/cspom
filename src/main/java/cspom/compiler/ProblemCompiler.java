package cspom.compiler;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Deque;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import cspom.CSPOM;
import cspom.constraint.CSPOMConstraint;
import cspom.constraint.FunctionalConstraint;
import cspom.constraint.GeneralConstraint;
import cspom.variable.BooleanDomain;
import cspom.variable.CSPOMVariable;

/**
 * This class implements some known useful reformulation rules.
 * 
 * @author vion
 * 
 */
public final class ProblemCompiler {

    private final CSPOM problem;
    private final Deque<CSPOMVariable> variables;
    private final Deque<CSPOMConstraint> constraints;
    private final AllDiffDetector allDiffDetector;

    private ProblemCompiler(final CSPOM problem) {
        this.problem = problem;
        variables = new LinkedList<CSPOMVariable>();
        constraints = new LinkedList<CSPOMConstraint>();
        allDiffDetector = new AllDiffDetector(problem);
    }

    public static void compile(final CSPOM problem) {
        new ProblemCompiler(problem).compile();
    }

    private void compile() {
        variables.addAll(problem.getVariables());
        constraints.addAll(problem.getConstraints());

        while (!variables.isEmpty() || !constraints.isEmpty()) {
            while (!variables.isEmpty()) {
                compileVariable(variables.poll());
            }

            while (!constraints.isEmpty()) {
                compileConstraint(constraints.poll());
            }
        }

    }

    public static void compileAllDiffs(final CSPOM problem) {
        new ProblemCompiler(problem).compileAllDiffs();
    }

    private void compileAllDiffs() {
        constraints.addAll(problem.getConstraints());

        while (!constraints.isEmpty()) {
            final CSPOMConstraint constraint = constraints.poll();
            if (!problem.getConstraints().contains(constraint)) {
                continue;
            }
            allDiffDetector.alldiff(constraint);
        }

    }

    private void compileVariable(final CSPOMVariable var) {
        deReify(var);
        removeSingle(var);
    }

    private void deReify(final CSPOMVariable v) {
        if (BooleanDomain.TRUE.equals(v.getDomain())) {
            for (CSPOMConstraint c : v.getConstraints()) {
                if (!(c instanceof FunctionalConstraint)) {
                    continue;
                }
                final FunctionalConstraint fc = (FunctionalConstraint) c;
                if (fc.getResultVariable() == v) {
                    deReify(v, fc);
                }
            }
        }
    }

    private void deReify(final CSPOMVariable trueVariable,
            final FunctionalConstraint constraint) {
        problem.removeConstraint(constraint);
        final CSPOMConstraint newConstraint = new GeneralConstraint(constraint
                .getDescription(), constraint.getParameters(), constraint
                .getArguments());
        problem.addConstraint(newConstraint);
        constraints.add(newConstraint);
        variables.addAll(constraint.getScope());
    }

    private void removeSingle(final CSPOMVariable var) {
        if (var.isAuxiliary() && var.getConstraints().isEmpty()) {
            problem.removeVariable(var);
        }
    }

    private void compileConstraint(final CSPOMConstraint constraint) {
        if (!problem.getConstraints().contains(constraint)) {
            return;
        }
        removeAnd(constraint);
        if (!problem.getConstraints().contains(constraint)) {
            return;
        }
        mergeEq(constraint);
        if (!problem.getConstraints().contains(constraint)) {
            return;
        }
        absdiff(constraint);
        if (!problem.getConstraints().contains(constraint)) {
            return;
        }
        diffGe(constraint);
        if (!problem.getConstraints().contains(constraint)) {
            return;
        }
        if (allDiffDetector.dropSubsumedDiff(constraint)) {
            variables.addAll(constraint.getScope());
        }
        if (!problem.getConstraints().contains(constraint)) {
            return;
        }
        final Collection<CSPOMVariable> changed = allDiffDetector
                .alldiff(constraint);
        if (changed != null) {
            variables.addAll(changed);
        }
    }

    private void removeAnd(final CSPOMConstraint constraint) {
        if ("and".equals(constraint.getDescription())
                && constraint instanceof GeneralConstraint) {
            for (CSPOMVariable v : constraint) {
                v.setDomain(BooleanDomain.TRUE);
                variables.add(v);
            }
            problem.removeConstraint(constraint);
        }
    }

    /**
     * If given constraint is an all-equal constraint, merges and removes all
     * auxiliary variables.
     * 
     * @param constraint
     */
    private void mergeEq(final CSPOMConstraint constraint) {
        if ("eq".equals(constraint.getDescription())
                && constraint instanceof GeneralConstraint) {

            final LinkedList<CSPOMVariable> scope = new LinkedList<CSPOMVariable>(
                    constraint.getScope());

            final Collection<CSPOMVariable> auxVars = new ArrayList<CSPOMVariable>();

            /*
             * Find auxiliary variables in the scope of the constraint.
             */
            for (final Iterator<CSPOMVariable> itr = scope.iterator(); itr
                    .hasNext();) {
                final CSPOMVariable var = itr.next();
                if (var.isAuxiliary()) {
                    auxVars.add(var);
                    itr.remove();
                }
            }

            /*
             * Do not merge original variables.
             */
            if (auxVars.isEmpty()) {
                return;
            }
            problem.removeConstraint(constraint);

            /*
             * Generate a new all-equal constraint if more than one variable
             * remains.
             */
            if (scope.size() > 1) {
                final CSPOMConstraint newConstraint = new GeneralConstraint(
                        "eq", null, scope.toArray(new CSPOMVariable[scope
                                .size()]));
                constraints.add(newConstraint);
                problem.addConstraint(newConstraint);
            }
            final CSPOMVariable refVar;
            if (scope.isEmpty()) {
                refVar = auxVars.iterator().next();
            } else {
                refVar = scope.getFirst();
            }

            for (CSPOMVariable aux : auxVars) {
                if (aux != refVar) {
                    merge(aux, refVar);
                }
            }
        }
    }

    private void merge(final CSPOMVariable merged, final CSPOMVariable variable) {
        if (merged == variable) {
            throw new IllegalArgumentException();
        }
        for (CSPOMConstraint c : merged.getConstraints()) {
            merged.removeConstraint(c);
            c.replaceVar(merged, variable);
            variable.registerConstraint(c);
        }
        problem.removeVariable(merged);
    }

    /**
     * If constraint is the sub() constraint, converts a=sub(y,z), x=abs(a) to
     * x=absdiff(y,z). No other constraint may imply the auxiliary constraint a.
     * 
     * @param constraint
     */
    private void absdiff(final CSPOMConstraint constraint) {
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
        variables.add(absConstraint.getResultVariable());
        for (CSPOMVariable v : subConstraint.getScope()) {
            variables.add(v);
        }
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

    /**
     * If constraint is the sub() constraint, converts a=sub(y,z), x=abs(a) to
     * x=absdiff(y,z). No other constraint may imply the auxiliary constraint a.
     * 
     * @param constraint
     */
    private void diffGe(final CSPOMConstraint constraint) {
        if (!("sub".equals(constraint.getDescription()) && constraint instanceof FunctionalConstraint)) {
            return;
        }
        final FunctionalConstraint subConstraint = (FunctionalConstraint) constraint;
        final CSPOMVariable result = subConstraint.getResultVariable();
        if (!result.isAuxiliary() || result.getConstraints().size() != 2) {
            return;
        }
        final FunctionalConstraint geConstraint = geConstraint(result);
        if (geConstraint == null) {
            return;
        }
        problem.removeConstraint(subConstraint);
        problem.removeConstraint(geConstraint);
        final CSPOMVariable[] scope = new CSPOMVariable[] {
                subConstraint.getVariable(1), subConstraint.getVariable(2),
                geConstraint.getVariable(2) };

        problem.addConstraint(new FunctionalConstraint(geConstraint
                .getResultVariable(), "diffGe", null, scope));
        variables.add(geConstraint.getResultVariable());
        for (CSPOMVariable v : subConstraint.getScope()) {
            variables.add(v);
        }
    }

    private static FunctionalConstraint geConstraint(
            final CSPOMVariable variable) {
        for (CSPOMConstraint c : variable.getConstraints()) {
            if ("ge".equals(c.getDescription())
                    && c instanceof FunctionalConstraint) {
                final FunctionalConstraint geConstraint = (FunctionalConstraint) c;
                final CSPOMVariable[] scope = geConstraint.getArguments();
                if (scope.length == 2 && scope[0] == variable) {
                    return geConstraint;
                }
            }
        }
        return null;
    }
}
