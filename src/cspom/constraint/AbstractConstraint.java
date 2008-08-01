package cspom.constraint;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import cspom.variable.Variable;

public abstract class AbstractConstraint implements Constraint {

	private final String name;

	private final List<Variable> scope;

	private final int arity;

	private final Map<Variable, Integer> positions;

	private final DomainSignature domainSignature;

	public AbstractConstraint(final String name, final List<Variable> scope) {
		this.scope = scope;
		this.name = name;
		arity = scope.size();
		positions = new HashMap<Variable, Integer>(arity);
		for (int i = arity; --i >= 0;) {
			positions.put(scope.get(i), i);
		}

		domainSignature = new DomainSignature();
		for (Variable v : scope) {
			domainSignature.add(v.getDomain());
		}
	}

	public List<Variable> getScope() {
		return scope;
	}

	public String getName() {
		return name;
	}

	public String toString() {
		return name + " (" + scope + ")";
	}

	public int getArity() {
		return arity;
	}

	public int getPosition(final Variable variable) {
		return positions.get(variable);
	}

	protected Map<Variable, Variable> newOrder(final List<Variable> scope) {
		assert scope.size() == arity;

		final Map<Variable, Variable> newOrder = new HashMap<Variable, Variable>(
				arity);

		for (int i = scope.size(); --i >= 0;) {
			newOrder.put(this.scope.get(i), scope.get(i));
		}

		return newOrder;
	}

	protected int position(final Variable variable) {
		return scope.indexOf(variable);
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
