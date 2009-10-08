package cspom.extension;

import java.util.HashMap;
import java.util.Map;

import cspom.constraint.AbstractConstraint;
import cspom.variable.CSPOMVariable;

public class ExtensionConstraint extends AbstractConstraint {

	private final Extension relation;

	public ExtensionConstraint(final String name, final Extension relation,
			final CSPOMVariable... scope) {
		super(name, "ext-" + relation.getName(), scope);
		this.relation = relation;
	}

	public ExtensionConstraint(final Extension relation,
			final CSPOMVariable... scope) {
		super(null, scope);
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

	public ExtensionConstraint standardize(final CSPOMVariable[] scope) {
		assert scope.length == getArity();
		final int[] newPosition = new int[getArity()];
		final Map<CSPOMVariable, CSPOMVariable> newOrder = new HashMap<CSPOMVariable, CSPOMVariable>(
				getArity());

		for (int i = scope.length; --i >= 0;) {
			newOrder.put(this.getScope()[i], scope[i]);
		}

		for (int i = getArity(); --i >= 0;) {
			newPosition[i] = getPosition(newOrder.get(getScope()[i]));
		}

		return new ExtensionConstraint(relation.reverse(newPosition), scope);
	}
}
