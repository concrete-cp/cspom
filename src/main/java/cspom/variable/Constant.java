package cspom.variable;

public class Constant extends CSPOMVariable {

	public Constant(final Number value) {
		super(value.toString(), new SingletonDomain(value));
	}
}
