package cspom.extension;

import java.util.List;
import java.util.Map;

import cspom.constraint.AbstractConstraint;
import cspom.variable.Variable;

public class ExtensionConstraint extends AbstractConstraint {

	private final Extension relation;

	public ExtensionConstraint(String name, List<Variable> scope,
			Extension relation) {
		super(name, scope);
		this.relation = relation;
	}

	public Extension getRelation() {
		return relation;
	}

	public String toString() {
		return super.toString() + ": " + relation;
	}

	public boolean evaluate(final Number[] numbers) {
		return relation.evaluate(numbers);
	}

	public ExtensionConstraint standardize(final List<Variable> scope) {
		final int[] newPosition = new int[getArity()];
		final Map<Variable, Variable> newOrder = newOrder(scope);

		for (int i = getArity(); --i >= 0;) {
			newPosition[i] = position(newOrder.get(getScope().get(i)));
		}

		return new ExtensionConstraint(getName(), scope, relation
				.reverse(newPosition));
	}

}
