package cspom.variable;

public final class Constant implements Domain {
	private final Number value;

	public Constant(Number value) {
		this.value = value;
	}

	public Number getValue() {
		return value;
	}

}
