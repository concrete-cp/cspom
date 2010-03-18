package cspom.compiler;

import java.util.Deque;
import java.util.Iterator;
import java.util.LinkedList;

import cspom.CSPOM;
import cspom.constraint.CSPOMConstraint;
import cspom.constraint.FunctionalConstraint;
import cspom.constraint.GeneralConstraint;
import cspom.variable.BooleanDomain;
import cspom.variable.CSPOMVariable;

public final class ProblemCompiler {
    private final CSPOM problem;
    private final Deque<CSPOMVariable> variables;
    private final Deque<CSPOMConstraint> constraints;

    public ProblemCompiler(final CSPOM problem) {
        this.problem = problem;
        variables = new LinkedList<CSPOMVariable>();
        constraints = new LinkedList<CSPOMConstraint>();

    }

    public void compile() {
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

    public void compileVariable(CSPOMVariable var) {
        if (deReify(var)) {
            removeSingle(var);
        }
    }

    public boolean deReify(CSPOMVariable v) {
        boolean change = false;
        if (BooleanDomain.TRUE.equals(v.getDomain())) {
            for (CSPOMConstraint c : v.getConstraints()) {
                if (!(c instanceof FunctionalConstraint)) {
                    continue;
                }
                final FunctionalConstraint fc = (FunctionalConstraint) c;
                if (fc.getResultVariable() == v) {
                    deReify(v, fc);
                    change = true;
                }
            }
        }

        return change;
    }

    private void deReify(final CSPOMVariable trueVariable,
            final FunctionalConstraint constraint) {
        problem.removeConstraint(constraint);
        constraints.remove(constraint);
        final CSPOMConstraint newConstraint = new GeneralConstraint(constraint
                .getDescription(), constraint.getArguments());
        problem.addConstraint(newConstraint);
        constraints.add(newConstraint);
    }

    private void removeSingle(final CSPOMVariable var) {
        if (var.isAuxiliary() && var.getConstraints().isEmpty()) {
            problem.removeVariable(var);
        }
    }

    private void compileConstraint(CSPOMConstraint constraint) {
        removeAnd(constraint);
        mergeEq(constraint);
    }

    private void removeAnd(CSPOMConstraint constraint) {
        if ("and".equals(constraint.getDescription())
                && (constraint instanceof GeneralConstraint)) {
            for (CSPOMVariable v : constraint) {
                v.setDomain(BooleanDomain.TRUE);
                variables.add(v);
            }
            problem.removeConstraint(constraint);
        }
    }

    private void mergeEq(CSPOMConstraint constraint) {
        if ("eq".equals(constraint.getDescription())
                && (constraint instanceof GeneralConstraint)) {

            final LinkedList<CSPOMVariable> scope = new LinkedList<CSPOMVariable>(
                    constraint.getScope());
            CSPOMVariable aux = null;
            for (Iterator<CSPOMVariable> itr = scope.iterator(); itr.hasNext();) {
                final CSPOMVariable var = itr.next();
                if (var.isAuxiliary()) {
                    aux = var;
                    itr.remove();
                    break;
                }
            }
            if (aux == null) {
                return;
            }
            problem.removeConstraint(constraint);

            if (scope.size() > 1) {
                final CSPOMConstraint newConstraint = new GeneralConstraint(
                        "eq", scope.toArray(new CSPOMVariable[scope.size()]));
                constraints.add(newConstraint);
                problem.addConstraint(newConstraint);
            }
            merge(aux, scope.getFirst());
        }
    }

    private void merge(CSPOMVariable merged, CSPOMVariable variable) {
        for (CSPOMConstraint c : merged.getConstraints()) {
            merged.removeConstraint(c);
            c.replaceVar(merged, variable);
            variable.registerConstraint(c);
            
        }
        problem.removeVariable(merged);
    }
}
