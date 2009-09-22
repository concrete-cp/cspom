package cspom.extension;

import java.util.Map;

import cspom.constraint.AbstractConstraint;
import cspom.variable.Variable;

public class ExtensionConstraint extends AbstractConstraint {

	private final Extension relation;

	public ExtensionConstraint(final String name, final Extension relation,
			final Variable... scope) {
		super(name, scope);
		this.relation = relation;
	}

	public ExtensionConstraint(final Extension relation,
			final Variable... scope) {
		super(scope);
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

	public ExtensionConstraint standardize(final Variable[] scope) {
		final int[] newPosition = new int[getArity()];
		final Map<Variable, Variable> newOrder = newOrder(scope);

		for (int i = getArity(); --i >= 0;) {
			newPosition[i] = position(newOrder.get(getScope()[i]));
		}

		return new ExtensionConstraint(relation.reverse(newPosition), scope);
	}

}
