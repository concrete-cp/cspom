package cspom.variable;


public class Variable {
	private final Domain domain;
	private final String name;

	public Variable(final String name, final Domain domain) {
		this.domain = domain;
		this.name = name;
	}

	public Domain getDomain() {
		return domain;
	}

	public String getName() {
		return name;
	}

	public String toString() {
		return name + " (" + domain + ")";
	}
}
