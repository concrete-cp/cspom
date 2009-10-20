package cspom.variable;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import cspom.constraint.CSPOMConstraint;

public final class CSPOMVariable {
	private static int noname = 0;
	private final String name;
	private boolean root = false;
	private final Set<CSPOMConstraint> constraints;
	private DomainType domainType;
	private Domain domain;

	public enum DomainType {
		UNKNOWN, UNKNOWN_INT, EXT_INT, INTERVAL_INT, BOOLEAN, CONSTANT;

		public boolean isInt() {
			return this == UNKNOWN_INT || this == EXT_INT
					|| this == INTERVAL_INT;
		}
	}

	public DomainType getDomainType() {
		return domainType;
	}

	public void setDomainType(DomainType dt) {
		this.domainType = dt;
	}

	public CSPOMVariable(final String name, final DomainType domainType) {
		this.domainType = domainType;
		this.name = name;
		constraints = new HashSet<CSPOMConstraint>();
	}

	public CSPOMVariable(final DomainType domainType) {
		this(generateName(), domainType);
	}

	public CSPOMVariable(final String name, final Interval interval) {
		this(name, DomainType.INTERVAL_INT);
		this.domain = interval;
	}

	public CSPOMVariable(final String name, final Number lB, final Number uB) {
		this(name, new Interval(lB, uB));
	}

	public CSPOMVariable(final Number lB, final Number uB) {
		this(generateName(), lB, uB);
	}

	public CSPOMVariable(final String name, final List<Number> values) {
		this(name, DomainType.EXT_INT);
		this.domain = new ExtensiveDomain(values);
	}

	public CSPOMVariable(final int constant) {
		this(DomainType.CONSTANT);
		this.domain = new Constant(constant);
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

	public boolean isRoot() {
		return root;
	}

	public void registerConstraint(CSPOMConstraint constraint) {
		constraints.add(constraint);
	}

	// public boolean deregisterConstraint(CSPOMConstraint constraint) {
	// return !constraints.remove(constraint);
	// }

	public Set<CSPOMConstraint> getConstraints() {
		return constraints;
	}

	private static String generateName() {
		return "Generated" + noname++;
	}
}
