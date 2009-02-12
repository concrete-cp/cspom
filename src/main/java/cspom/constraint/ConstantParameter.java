package cspom.constraint;



public class ConstantParameter implements Parameter {

	private final String name;

	private final Number constant;

	public final static String PATTERN = "-?\\+?\\ *\\d*\\.?\\d*e?-?\\+?\\d";

	public ConstantParameter(final String name, final Number constant) {
		this.constant = constant;
		this.name = name;
	}

	public String toString() {
		return name + "=" + constant;
	}

	public Number getNumber() {
		return constant;
	}

	public String getName() {
		return name;
	}

}
