package cspom;

import cspom.variable.DomainType;

public class Signature {
	private final String constraint;
	private final DomainType[] types;

	public Signature(String constraint, DomainType... types) {
		this.constraint = constraint;
		this.types = types;
	}

	public String getConstraint() {
		return constraint;
	}

	public DomainType[] getTypes() {
		return types;
	}
}
