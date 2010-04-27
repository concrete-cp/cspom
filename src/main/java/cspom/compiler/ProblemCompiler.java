package cspom.compiler;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Deque;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

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

	private static final ConstraintSelector DIFF_CONSTRAINT = new ConstraintSelector() {
		@Override
		public boolean is(final CSPOMConstraint constraint) {
			return constraint instanceof GeneralConstraint
					&& "ne".equals(constraint.getDescription())
					|| "gt".equals(constraint.getDescription())
					|| "lt".equals(constraint.getDescription())
					|| "allDifferent".equals(constraint.getDescription());
		}
	};

	private static final ConstraintSelector ALLDIFF_CONSTRAINT = new ConstraintSelector() {
		@Override
		public boolean is(final CSPOMConstraint constraint) {
			return constraint instanceof GeneralConstraint
					&& "ne".equals(constraint.getDescription())
					|| "allDifferent".equals(constraint.getDescription());
		}
	};

	private final CSPOM problem;
	private final Deque<CSPOMVariable> variables;
	private final Deque<CSPOMConstraint> constraints;

	private ProblemCompiler(final CSPOM problem) {
		this.problem = problem;
		variables = new LinkedList<CSPOMVariable>();
		constraints = new LinkedList<CSPOMConstraint>();
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
			alldiff(constraint);
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
		dropSubsumedDiff(constraint);
		if (!problem.getConstraints().contains(constraint)) {
			return;
		}
		alldiff(constraint);
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
			if (scope.size() <= 1) {
				return;
			}
			final CSPOMVariable refVar;
			if (scope.isEmpty()) {
				refVar = auxVars.iterator().next();
			} else {
				refVar = scope.getFirst();
			}

			final CSPOMConstraint newConstraint = new GeneralConstraint("eq",
					null, scope.toArray(new CSPOMVariable[scope.size()]));
			constraints.add(newConstraint);
			problem.addConstraint(newConstraint);

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
	 * If constraint is part of a larger clique of inequalities, replace it by a
	 * larger all-diff constraint.
	 * 
	 * @param constraint
	 */
	private void alldiff(final CSPOMConstraint constraint) {
		if (!DIFF_CONSTRAINT.is(constraint)) {
			return;
		}
		System.out.print(constraint);
		// Lowest degree first (fewer variables to try)
		final SortedSet<CSPOMVariable> clique = new TreeSet<CSPOMVariable>(
				new Comparator<CSPOMVariable>() {
					@Override
					public int compare(final CSPOMVariable o1,
							final CSPOMVariable o2) {
						final int degreeComp = o1.getConstraints().size()
								- o2.getConstraints().size();
						if (degreeComp == 0) {
							return o1.getName().compareTo(o2.getName());
						}
						return degreeComp;
					}
				});

		clique.addAll(constraint.getScope());

		// Highest degree first (likely to obtain a larger clique)
		final SortedSet<CSPOMVariable> candidates = new TreeSet<CSPOMVariable>(
				new Comparator<CSPOMVariable>() {
					@Override
					public int compare(final CSPOMVariable o1,
							final CSPOMVariable o2) {
						final int degreeComp = o2.getConstraints().size()
								- o1.getConstraints().size();
						if (degreeComp == 0) {
							return o1.getName().compareTo(o2.getName());
						}
						return degreeComp;
					}
				});
		for (CSPOMConstraint c : clique.first().getConstraints()) {
			for (CSPOMVariable v : c.getScope()) {
				if (!clique.contains(v)) {
					candidates.add(v);
				}
			}
		}

		for (CSPOMVariable v : candidates) {
			if (CliqueDetector.allEdges(v, clique, DIFF_CONSTRAINT)) {
				clique.add(v);
			}
		}

		if (clique.size() > constraint.getScope().size()) {
			System.out.print(" -> " + clique);
			int rem = newAllDiff(clique);
			System.out.print(", " + rem + " constraints removed, "
					+ problem.getConstraints().size() + " remaining");
		}
		System.out.println();
	}

	/**
	 * Adds a new all-diff constraint of the specified scope. Any newly subsumed
	 * neq/all-diff constraints are removed.
	 * 
	 * @param scope
	 */
	private int newAllDiff(final Collection<CSPOMVariable> scope) {
		final CSPOMConstraint allDiff = new GeneralConstraint("allDifferent",
				null, scope.toArray(new CSPOMVariable[scope.size()]));
		problem.addConstraint(allDiff);
		// constraints.add(allDiff);
		variables.addAll(scope);

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
			for (CSPOMVariable v : c.getScope()) {
				if (!scope.contains(v)) {
					variables.add(v);
				}
			}
		}
		return subsumed.size();
	}

	/**
	 * If the given constraint is an all-different or neq constraint, remove it
	 * if it is subsumed by another difference constraint.
	 * 
	 * @param constraint
	 */
	private void dropSubsumedDiff(final CSPOMConstraint constraint) {
		if (ALLDIFF_CONSTRAINT.is(constraint)
				&& CliqueDetector.haveSubsumingConstraint(constraint,
						DIFF_CONSTRAINT)) {
			variables.addAll(constraint.getScope());
			problem.removeConstraint(constraint);
		}
	}

}
