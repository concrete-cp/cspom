package cspom.compiler;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Deque;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

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

			alldiff();
		}

	}

	public void compileVariable(final CSPOMVariable var) {
		deReify(var);
		removeSingle(var);
	}

	public boolean deReify(final CSPOMVariable v) {
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
				.getDescription(), constraint.getParameters(), constraint
				.getArguments());
		problem.addConstraint(newConstraint);
		constraints.add(newConstraint);
	}

	private void removeSingle(final CSPOMVariable var) {
		if (var.isAuxiliary() && var.getConstraints().isEmpty()) {
			problem.removeVariable(var);
		}
	}

	private void compileConstraint(final CSPOMConstraint constraint) {
		removeAnd(constraint);
		mergeEq(constraint);
		absdiff(constraint);
	}

	private void removeAnd(final CSPOMConstraint constraint) {
		if ("and".equals(constraint.getDescription())
				&& (constraint instanceof GeneralConstraint)) {
			for (CSPOMVariable v : constraint) {
				v.setDomain(BooleanDomain.TRUE);
				variables.add(v);
			}
			problem.removeConstraint(constraint);
		}
	}

	private void mergeEq(final CSPOMConstraint constraint) {
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
						"eq", null, scope.toArray(new CSPOMVariable[scope
								.size()]));
				constraints.add(newConstraint);
				problem.addConstraint(newConstraint);
			}
			merge(aux, scope.getFirst());
		}
	}

	private void merge(final CSPOMVariable merged, final CSPOMVariable variable) {
		for (CSPOMConstraint c : merged.getConstraints()) {
			merged.removeConstraint(c);
			c.replaceVar(merged, variable);
			variable.registerConstraint(c);

		}
		problem.removeVariable(merged);
	}

	private void absdiff(final CSPOMConstraint constraint) {
		if (!"sub".equals(constraint.getDescription())
				|| !(constraint instanceof FunctionalConstraint)) {
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
		constraints.remove(absConstraint);
		problem.addConstraint(new FunctionalConstraint(absConstraint
				.getResultVariable(), "absdiff", null, subConstraint
				.getArguments()));
		variables.add(absConstraint.getResultVariable());
		for (CSPOMVariable v : subConstraint.getScope()) {
			variables.add(v);
		}
	}

	private FunctionalConstraint absConstraint(final CSPOMVariable variable) {
		for (CSPOMConstraint c : variable.getConstraints()) {
			if ("abs".equals(c.getDescription())
					&& (c instanceof FunctionalConstraint)) {
				final FunctionalConstraint fConstraint = (FunctionalConstraint) c;
				if (Arrays.equals(new CSPOMVariable[] { variable }, fConstraint
						.getArguments())) {
					return fConstraint;
				}
			}
		}
		return null;
	}

	private List<CSPOMVariable> enlargeClique(final List<CSPOMVariable> clique,
			final int startWith, final ConstraintValidator validator) {
		List<CSPOMVariable> largest = new ArrayList<CSPOMVariable>(clique);
		System.out.println(clique);
		for (int i = startWith; i < problem.getVariables().size(); i++) {
			final CSPOMVariable v = problem.getVariables().get(i);
			if (allEdges(v, clique, validator)) {
				clique.add(v);
				final List<CSPOMVariable> larger = enlargeClique(clique, i + 1,
						validator);
				if (larger.size() > largest.size()) {
					largest = larger;
				}
				clique.remove(v);
			}
		}
		return largest;
	}

	private boolean allEdges(final CSPOMVariable var,
			final Collection<CSPOMVariable> vars,
			final ConstraintValidator validator) {
		for (CSPOMVariable v : vars) {
			if (edge(v, var, validator) == null) {
				return false;
			}
		}
		return true;
	}

	private static CSPOMConstraint edge(final CSPOMVariable var1,
			final CSPOMVariable var2, final ConstraintValidator validator) {
		final Set<CSPOMConstraint> constraints = var1.getConstraints();

		for (CSPOMConstraint c : var2.getConstraints()) {
			if (validator.validate(c) && constraints.contains(c)) {
				return c;
			}
		}

		return null;

	}

	private static interface ConstraintValidator {
		boolean validate(CSPOMConstraint constraint);
	}

	private static final ConstraintValidator DIFF_CONSTRAINT = new ConstraintValidator() {
		@Override
		public boolean validate(final CSPOMConstraint constraint) {
			if (!(constraint instanceof GeneralConstraint)) {
				return false;
			}
			return "ne".equals(constraint.getDescription())
					|| "gt".equals(constraint.getDescription())
					|| "lt".equals(constraint.getDescription());
		}
	};

	private void alldiff() {
		for (;;) {
			final List<CSPOMVariable> largest = enlargeClique(
					new ArrayList<CSPOMVariable>(), 0, DIFF_CONSTRAINT);

			if (largest.size() <= 2) {
				break;
			}

			for (int i = largest.size(); --i >= 0;) {
				for (int j = i; --j >= 0;) {
					final CSPOMConstraint neq = edge(largest.get(i), largest
							.get(j), new ConstraintValidator() {
						@Override
						public boolean validate(final CSPOMConstraint constraint) {
							return (constraint instanceof GeneralConstraint && "ne"
									.equals(constraint.getDescription()));
						}
					});
					problem.removeConstraint(neq);
				}
			}

			final CSPOMConstraint allDiff = new GeneralConstraint(
					"allDifferent", null, largest
							.toArray(new CSPOMVariable[largest.size()]));
			problem.addConstraint(allDiff);
			constraints.add(allDiff);
			variables.addAll(largest);
		}
	}
}
