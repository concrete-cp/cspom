package cspom.variable;

import java.util.List;

public class UnknownDomain implements Domain {

	@Override
	public String getName() {
		return null;
	}

	@Override
	public int getNbValues() {
		throw new UnsupportedOperationException();
	}

	public List<Number> getValues() {
		throw new UnsupportedOperationException();
	}

}
