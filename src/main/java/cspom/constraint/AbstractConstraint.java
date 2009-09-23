package cspom.constraint;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import cspom.variable.Variable;

public abstract class AbstractConstraint implements Constraint {

	private final String name;

	private final Variable[] scope;

	private final int arity;

	private final Map<Variable, Integer> positions;

	public AbstractConstraint(final Variable... scope) {
		this(null, scope);
	}

	public AbstractConstraint(final String name, final Variable... scope) {
		this.scope = scope;
		this.name = name;
		arity = scope.length;
		positions = new HashMap<Variable, Integer>(arity);
		for (int i = arity; --i >= 0;) {
			positions.put(scope[i], i);
		}
	}

	public Variable[] getScope() {
		return scope;
	}

	public String toString() {
		return "(" + Arrays.toString(scope) + ")";
	}

	public int getArity() {
		return arity;
	}

	public int getPosition(final Variable variable) {
		return positions.get(variable);
	}

	protected int position(final Variable variable) {
		for (int i = scope.length; --i >= 0;) {
			if (scope[i] == variable) {
				return i;
			}
		}
		return -1;
	}

	public int hashCode() {
		return Arrays.hashCode(scope);
	}
}
