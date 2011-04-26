package cspom.compiler.patterns;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Random;
import java.util.Set;

import cspom.compiler.ConstraintSelector;
import cspom.constraint.CSPOMConstraint;
import cspom.constraint.GeneralConstraint;
import cspom.CSPOM;
import cspom.variable.CSPOMVariable;

public final class AllDiff implements ConstraintCompiler {

    private static final ConstraintSelector DIFF_CONSTRAINT = new ConstraintSelector() {
        @Override
        public boolean is(final CSPOMConstraint constraint) {
            return constraint instanceof GeneralConstraint
                    && "ne".equals(constraint.description())
                    || "gt".equals(constraint.description())
                    || "lt".equals(constraint.description())
                    || "allDifferent".equals(constraint.description());
        }
    };

    private static final ConstraintSelector ALLDIFF_CONSTRAINT = new ConstraintSelector() {
        @Override
        public boolean is(final CSPOMConstraint constraint) {
            return constraint instanceof GeneralConstraint
                    && "ne".equals(constraint.description())
                    || "allDifferent".equals(constraint.description());
        }
    };

    private static final int ITER = 750;
    private static final Random RAND = new Random(0);

    private final CSPOM problem;
    private final CliqueDetector cliqueDetector;

    public AllDiff(final CSPOM problem) {
        this.problem = problem;
        this.cliqueDetector = new CliqueDetector();
    }

    /**
     * If constraint is part of a larger clique of inequalities, replace it by a
     * larger all-diff constraint.
     * 
     * @param constraint
     */
    public void alldiff(final CSPOMConstraint constraint) {
        if (!DIFF_CONSTRAINT.is(constraint)) {
            return;
        }
        // System.out.print(constraint);

        final Set<CSPOMVariable<?>> pool = new HashSet<CSPOMVariable<?>>();
        final Set<CSPOMVariable<?>> clique = new HashSet<CSPOMVariable<?>>(
                JavaCollections.asJavaList(constraint.scope()));
        populate(pool, clique);
        expand(clique, pool);

        if (clique.size() > constraint.scope().size()) {
            // System.out.print(" -> " + clique.size() + "-clique");
            newAllDiff(clique);
            // System.out.print(", " + (rem.size() - 1) +
            // " constraints removed, "
            // + problem.getConstraints().size() + " remaining");
        }
        // System.out.println();

    }

    private void populate(final Set<CSPOMVariable<?>> pool,
            final Collection<CSPOMVariable<?>> base) {
        final Iterator<CSPOMVariable> baseItr = base.iterator();
        if (!baseItr.hasNext()) {
            return;
        }
        final CSPOMVariable first = baseItr.next();
        for (CSPOMConstraint c : first.getConstraints()) {
            if (DIFF_CONSTRAINT.is(c)) {
                for (CSPOMVariable n : c.getScope()) {
                    if (n != first) {
                        pool.add(n);
                    }
                }
            }
        }
        pool.removeAll(base);
        while (baseItr.hasNext()) {
            final CSPOMVariable variable = baseItr.next();
            for (Iterator<CSPOMVariable> poolItr = pool.iterator(); poolItr
                    .hasNext();) {
                final CSPOMVariable poolVar = poolItr.next();
                if (!cliqueDetector.edge(variable, poolVar, DIFF_CONSTRAINT)) {
                    poolItr.remove();
                }
            }
        }
    }

    private static final int TABU_SIZE = 15;

    private static CSPOMVariable<?> pick(final Set<CSPOMVariable<?>> pool,
            final Map<CSPOMVariable<?>, Integer> tabu, final int iteration) {
        int tie = 1;
        CSPOMVariable returned = null;
        for (CSPOMVariable v : pool) {
            final Integer tabuLength = tabu.get(v);
            if ((tabuLength == null || tabuLength > iteration)
                    && RAND.nextFloat() * tie++ < 1) {
                returned = v;
            }
        }

        if (returned == null) {
            return null;
        }
        tabu.put(returned, iteration - TABU_SIZE);
        return returned;
    }

    private void expand(final Set<CSPOMVariable<?>> clique,
            final Set<CSPOMVariable<?>> pool) {
        final Set<CSPOMVariable> largest = new HashSet<CSPOMVariable>(clique);
        final Set<CSPOMVariable> base = new HashSet<CSPOMVariable>(clique);
        final Map<CSPOMVariable, Integer> tabu = new HashMap<CSPOMVariable, Integer>();

        for (int i = ITER; --i >= 0;) {
            final CSPOMVariable newVar = pick(pool, tabu, i);
            if (newVar == null) {
                if (clique.size() - base.size() <= 0) {
                    break;
                }
                int rand = RAND.nextInt(clique.size() - base.size());
                CSPOMVariable toRemove = null;
                for (CSPOMVariable v : clique) {
                    if (!base.contains(v) && --rand < 0) {
                        toRemove = v;
                        break;
                    }
                }
                clique.remove(toRemove);
                pool.clear();
                populate(pool, clique);
            } else {
                clique.add(newVar);
                if (clique.size() > largest.size()) {
                    largest.clear();
                    largest.addAll(clique);
                }
                pool.remove(newVar);
                for (Iterator<CSPOMVariable> itr = pool.iterator(); itr
                        .hasNext();) {
                    if (!cliqueDetector.edge(itr.next(), newVar,
                            DIFF_CONSTRAINT)) {
                        itr.remove();
                    }
                }
            }
        }

        clique.clear();
        clique.addAll(largest);
    }

    /**
     * Adds a new all-diff constraint of the specified scope. Any newly subsumed
     * neq/all-diff constraints are removed.
     * 
     * @param scope
     */
    public void newAllDiff(final Collection<CSPOMVariable<?>> scope) {
        final CSPOMConstraint allDiff = new GeneralConstraint("allDifferent",
                null, scope.toArray(new CSPOMVariable[scope.size()]));
        if (problem.getConstraints().contains(allDiff)) {
            return;
        }
        problem.addConstraint(allDiff);

        /*
         * Remove newly subsumed neq/alldiff constraints.
         */
        final Set<CSPOMConstraint> subsumed = new HashSet<CSPOMConstraint>();
        for (CSPOMVariable v : scope) {
            for (CSPOMConstraint c : v.getConstraints()) {
                if (c != allDiff && ALLDIFF_CONSTRAINT.is(c)) {
                    subsumed.add(c);
                }
            }
        }

        for (Iterator<CSPOMConstraint> itr = subsumed.iterator(); itr.hasNext();) {
            final CSPOMConstraint candidate = itr.next();
            if (!CliqueDetector.subsumes(allDiff, candidate)) {
                itr.remove();
            }
        }
        for (CSPOMConstraint c : subsumed) {
            problem.removeConstraint(c);
        }
    }

    /**
     * If the given constraint is an all-different or neq constraint, remove it
     * if it is subsumed by another difference constraint.
     * 
     * @param constraint
     */
    public boolean dropSubsumedDiff(final CSPOMConstraint constraint) {
        if (ALLDIFF_CONSTRAINT.is(constraint)
                && CliqueDetector.haveSubsumingConstraint(constraint,
                        DIFF_CONSTRAINT)) {
            problem.removeConstraint(constraint);
            return true;
        }
        return false;
    }

    @Override
    public void compile(final CSPOMConstraint constraint) {
        if (!dropSubsumedDiff(constraint)) {
            alldiff(constraint);
        }

    }

}
