package cspom.variable;

public class Variable {
	private final Domain domain;
	private final String name;

	public Variable(final String name, final Domain domain) {
		this.domain = domain;
		this.name = name;
	}

	public Variable(final Domain domain) {
		this(null, domain);
	}

	public Variable(final String name, final int lB, final int uB) {
		this(name, new IntervalDomain("dom(" + name + ")", lB, uB));
	}

	public Variable(final int lB, final int uB) {
		this(null, lB, uB);
	}

	public Domain getDomain() {
		return domain;
	}

	public String getName() {
		return name;
	}

	public String toString() {
		return name;
	}
}
