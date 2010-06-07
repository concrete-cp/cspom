package cspom.compiler.patterns;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Deque;
import java.util.Iterator;
import java.util.LinkedList;

import cspom.CSPOM;
import cspom.constraint.CSPOMConstraint;
import cspom.constraint.GeneralConstraint;
import cspom.variable.CSPOMDomain;
import cspom.variable.CSPOMVariable;

/**
 * If given constraint is an all-equal constraint, merges and removes all
 * auxiliary variables.
 */
public final class MergeEq implements ConstraintCompiler {

	private final CSPOM problem;
	private final Deque<CSPOMConstraint> constraints;

	public MergeEq(final CSPOM problem, final Deque<CSPOMConstraint> constraints) {
		this.problem = problem;
		this.constraints = constraints;

	}

	@Override
	public void compile(final CSPOMConstraint constraint) {
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

	private <T> CSPOMDomain<T> mergeDomain(final CSPOMDomain<T> d0,
			final CSPOMDomain<T> d1) {
		if (d0 == null) {
			return d1;
		}
		if (d1 == null) {
			return d0;
		}
	}

	private void merge(final CSPOMVariable merged, final CSPOMVariable variable) {
		if (merged == variable) {
			throw new IllegalArgumentException();
		}

		if (variable.getDomain() == null) {
			variable.setDomain(merged.getDomain());
		} else {
			mergeDomains(merged.getDomain(), variable.getDomain());
		}

		for (CSPOMConstraint c : merged.getConstraints().toArray(
				new CSPOMConstraint[merged.getConstraints().size()])) {
			merged.removeConstraint(c);
			c.replaceVar(merged, variable);
			variable.registerConstraint(c);
		}
		problem.removeVariable(merged);
	}

}
