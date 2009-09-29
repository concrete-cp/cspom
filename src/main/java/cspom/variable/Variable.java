package cspom.variable;

public class Variable {
	private static int noname = 0;
	private final Domain domain;
	private final String name;
	private boolean root = false;

	public Variable(final String name, final Domain domain) {
		this.domain = domain;
		this.name = name;
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

	public String getName() {
		return name;
	}

	public String toString() {
		return name;
	}
	
	public void setRoot() {
		root = true;
	}

	private static String generateName() {
		return "Generated" + noname++;
	}
}
