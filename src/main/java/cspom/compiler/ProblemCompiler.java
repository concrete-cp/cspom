package cspom.compiler;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Deque;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Set;

import cspom.CSPOM;
import cspom.compiler.patterns.AbsDiff;
import cspom.compiler.patterns.AllDiff;
import cspom.compiler.patterns.ConstraintCompiler;
import cspom.compiler.patterns.DeReify;
import cspom.compiler.patterns.DiffGe;
import cspom.compiler.patterns.MergeEq;
import cspom.compiler.patterns.RemoveAnd;
import cspom.constraint.CSPOMConstraint;
import cspom.variable.CSPOMVariable;

/**
 * This class implements some known useful reformulation rules.
 * 
 * @author vion
 * 
 */
public final class ProblemCompiler {

    private final CSPOM problem;
    private final Deque<CSPOMConstraint> constraints = new LinkedList<CSPOMConstraint>() {

        /**
         * 
         */
        private static final long serialVersionUID = 1L;

        private final Set<CSPOMConstraint> present = new HashSet<CSPOMConstraint>();

        @Override
        public boolean add(final CSPOMConstraint e) {
            if (present.contains(e)) {
                return false;
            }
            return super.add(e);
        }

        @Override
        public CSPOMConstraint poll() {
            final CSPOMConstraint constraint = super.poll();
            present.remove(constraint);
            return constraint;
        }

    };

    private final Collection<ConstraintCompiler> constraintCompilers;

    private ProblemCompiler(final CSPOM problem) {
        this.problem = problem;
        constraintCompilers = Arrays.asList(new RemoveAnd(problem),
                new MergeEq(problem, constraints), new AllDiff(problem),
                new DiffGe(problem), new AbsDiff(problem), new DeReify(problem,
                        constraints));
    }

    public static void compile(final CSPOM problem) {
        new ProblemCompiler(problem).compile();
    }

    private void compile() {
        constraints.addAll(problem.getConstraints());

        while (!constraints.isEmpty()) {
            compileConstraint(constraints.poll());
        }

        final Collection<CSPOMVariable> singletons = new ArrayList<CSPOMVariable>();
        for (CSPOMVariable v : problem.getVariables()) {
            if (v.isAuxiliary() && v.getConstraints().isEmpty()) {
                singletons.add(v);
            }
        }

        for (CSPOMVariable v : singletons) {
            problem.removeVariable(v);
        }

    }

    private void compileConstraint(final CSPOMConstraint constraint) {
        for (ConstraintCompiler cc : constraintCompilers) {
            if (!problem.getConstraints().contains(constraint)) {
                return;
            }
            cc.compile(constraint);
        }
    }

}
