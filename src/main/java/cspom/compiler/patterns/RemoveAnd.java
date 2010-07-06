package cspom.compiler.patterns;

import java.util.Deque;

import cspom.CSPOM;
import cspom.constraint.CSPOMConstraint;
import cspom.constraint.GeneralConstraint;
import cspom.variable.BooleanDomain;
import cspom.variable.CSPOMVariable;

public final class RemoveAnd implements ConstraintCompiler {

	private final CSPOM problem;
	private final Deque<CSPOMConstraint> constraints;

	public RemoveAnd(final CSPOM problem,
			final Deque<CSPOMConstraint> constraints) {
		this.problem = problem;
		this.constraints = constraints;
	}

	@Override
	public void compile(final CSPOMConstraint constraint) {
		if ("and".equals(constraint.getDescription())
				&& constraint instanceof GeneralConstraint) {
			for (CSPOMVariable v : constraint) {
				if (BooleanDomain.DOMAIN.equals(v.getDomain())) {
					v.setDomain(BooleanDomain.TRUE);
					for (CSPOMConstraint c : v.getConstraints()) {
						if (c != constraint) {
							constraints.add(c);
						}
					}
				}
			}
			problem.removeConstraint(constraint);

		}
	}

}