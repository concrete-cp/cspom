package cspom.compiler;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import cspom.constraint.CSPOMConstraint;
import cspom.variable.CSPOMVariable;

public final class CliqueDetector {

	private static final class Pair {
		private final CSPOMVariable v1, v2;

		private Pair(final CSPOMVariable v1, final CSPOMVariable v2) {
			this.v1 = v1;
			this.v2 = v2;
		}

		public int hashCode() {
			return v1.hashCode() + v2.hashCode();
		}

		public boolean equals(final Object obj) {
			final Pair p2 = (Pair) obj;
			return (p2.v1 == v1 && p2.v2 == v2) || (p2.v2 == v1 && p2.v1 == v2);
		}
	}

	private final Map<Pair, Boolean> haveEdge;

	public CliqueDetector() {
		haveEdge = new HashMap<Pair, Boolean>();
	}

	public static boolean haveSubsumingConstraint(
			final CSPOMConstraint constraint, final ConstraintSelector validator) {
		for (CSPOMConstraint c : constraint.getScope().get(0).getConstraints()) {
			if (c != constraint && validator.is(c) && subsumes(c, constraint)) {
				return true;
			}
		}
		return false;
	}

	public static boolean subsumes(final CSPOMConstraint constraint,
			final CSPOMConstraint subsumed) {
		return constraint.involvesAll(subsumed.getScope());
	}

	public boolean allEdges(final CSPOMVariable var,
			final Collection<CSPOMVariable> vars,
			final ConstraintSelector validator) {
		for (CSPOMVariable v : vars) {
			if (!edge(v, var, validator)) {
				return false;
			}
		}
		return true;
	}

	public boolean edge(final CSPOMVariable var1, final CSPOMVariable var2,
			final ConstraintSelector validator) {
		final Pair pair = new Pair(var1, var2);
		final Boolean edge = haveEdge.get(pair);

		if (edge == null) {
			final Set<CSPOMConstraint> constraints = var2.getConstraints();

			for (CSPOMConstraint c : var1.getConstraints()) {
				if (validator.is(c) && constraints.contains(c)) {
					haveEdge.put(pair, true);
					return true;
				}
			}

			haveEdge.put(pair, false);
			return false;
		}
		return edge;
	}

}
