package cspom.compiler.patterns;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Deque;
import java.util.Iterator;
import java.util.LinkedList;

import com.google.common.collect.Iterables;

import scala.collection.JavaConversions;

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
		if ("eq".equals(constraint.description())
				&& constraint instanceof GeneralConstraint) {

		}

		compileT(constraint);

	}

	private <T> void compileT(final CSPOMConstraint constraint) {

		final LinkedList<CSPOMVariable<T>> scope = new LinkedList<CSPOMVariable<T>>(
				JavaConversions.asJavaCollection(constraint.scope()));

		final Collection<CSPOMVariable<T>> auxVars = new ArrayList<CSPOMVariable<T>>();

		/*
		 * Find auxiliary variables in the scope of the constraint.
		 */
		for (final Iterator<CSPOMVariable<T>> itr = scope.iterator(); itr
				.hasNext();) {
			final CSPOMVariable<T> var = itr.next();
			if (var.auxiliary()) {
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
			final CSPOMConstraint newConstraint = new GeneralConstraint("eq",
					null, scope);
			constraints.add(newConstraint);
			problem.addConstraint(newConstraint);
		}

		/*
		 * Update the constraints of the problem
		 */
		final CSPOMVariable<T> refVar;
		if (scope.isEmpty()) {
			refVar = Iterables.getFirst(auxVars, null);
		} else {
			refVar = scope.getFirst();
		}

		for (CSPOMVariable<T> aux : auxVars) {
			if (aux != refVar) {
				merge(aux, refVar);
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
		return d0.intersect(d1);
	}

	private <T> void merge(final CSPOMVariable<T> merged,
			final CSPOMVariable<T> variable) {
		if (merged == variable) {
			throw new IllegalArgumentException();
		}

		variable.domain_$eq(mergeDomain(merged.domain(), variable.domain()));

		for (CSPOMConstraint c : new ArrayList<CSPOMConstraint>(
				JavaConversions.asJavaCollection(merged.constraints()))) {
			problem.removeConstraint(c);
			problem.addConstraint(c.replacedVar(merged, variable));
		}
		problem.removeVariable(merged);
	}

}
