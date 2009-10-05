package cspom.variable;

import java.util.HashSet;
import java.util.Set;

import cspom.constraint.Constraint;

public class Variable {
	private static int noname = 0;
	private Domain domain;
	private final String name;
	private boolean root = false;
	private final Set<Constraint> constraints;

	public Variable(final String name, final Domain domain) {
		this.domain = domain;
		this.name = name;
		constraints = new HashSet<Constraint>();
	}

	public Variable(final Domain domain) {
		this(generateName(), domain);
	}

	public Variable(final String name, final int lB, final int uB) {
		this(name, new IntervalDomain("dom(" + name + ")", lB, uB));
	}

	public Variable(final int lB, final int uB) {
		this(generateName(), lB, uB);
	}

	public Domain getDomain() {
		return domain;
	}

	public void setDomain(Domain domain) {
		this.domain = domain;
	}

	public String getName() {
		return name;
	}

	public String toString() {
		return name;
	}

	public void setRoot() {
		root = true;
	}

	public void registerConstraint(Constraint constraint) {
		if (constraint.getPosition(this) < 0) {
			throw new IllegalArgumentException(this
					+ " should be in the scope of " + constraint);
		}
		constraints.add(constraint);
	}

	public boolean deregisterConstraint(Constraint constraint) {
		return !constraints.remove(constraint);
	}

	public Set<Constraint> getConstraints() {
		return constraints;
	}

	private static String generateName() {
		return "Generated" + noname++;
	}
}
