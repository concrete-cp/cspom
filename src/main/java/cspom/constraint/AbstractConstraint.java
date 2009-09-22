package cspom.constraint;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import cspom.variable.Variable;

public abstract class AbstractConstraint implements Constraint {

	private final String name;

	private final Variable[] scope;

	private final int arity;

	private final Map<Variable, Integer> positions;

	private final DomainSignature domainSignature;

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

		domainSignature = new DomainSignature();
		for (Variable v : scope) {
			domainSignature.add(v.getDomain());
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

	protected Map<Variable, Variable> newOrder(final Variable[] scope) {
		assert scope.length == arity;

		final Map<Variable, Variable> newOrder = new HashMap<Variable, Variable>(
				arity);

		for (int i = scope.length; --i >= 0;) {
			newOrder.put(this.scope[i], scope[i]);
		}

		return newOrder;
	}

	protected int position(final Variable variable) {
		for (int i = scope.length; --i >= 0;) {
			if (scope[i] == variable) {
				return i;
			}
		}
		return -1;
	}

	public DomainSignature signature() {
		return domainSignature;
	}

	public static List<String> parse(final String parameters) {
		final Pattern pattern = Pattern.compile(ConstantParameter.PATTERN);
		final Matcher matcher = pattern.matcher(parameters);

		final List<String> params = new ArrayList<String>();

		while (matcher.find()) {
			params.add(matcher.group());
		}

		return params;
	}
}
