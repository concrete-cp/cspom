package cspom.constraint;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import cspom.Relation;
import cspom.variable.Variable;

public class GlobalConstraint extends AbstractConstraint {
	private final String type;

	public GlobalConstraint(String name, List<Variable> scope, String type) {
		super(name, scope);
		this.type = type;
	}

	public String getType() {
		return type;
	}

	public Relation getRelation() {
		throw new IllegalArgumentException();
	}

	public boolean evaluate(final Number[] numbers) {
		if ("allDifferent".equals(type)) {
			final Set<Number> union = new HashSet<Number>();
			for (Number n : numbers) {
				if (union.contains(n)) {
					return false;
				}
				union.add(n);
			}
		}
		return true;
	}

	public Constraint standardize(final List<Variable> scope) {
		throw new IllegalArgumentException();
	}

	public String toString() {
		return super.toString() + ": " + type;
	}
}
