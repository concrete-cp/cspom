package cspom.variable;

public class Constant extends Variable {

	public Constant(final Number value) {
		super(value.toString(), new SingletonDomain(value));
	}
}
