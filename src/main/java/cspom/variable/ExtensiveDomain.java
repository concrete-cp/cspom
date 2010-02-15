package cspom.variable;

import java.util.Arrays;
import java.util.List;

public final class ExtensiveDomain implements Domain {
	private final List<Number> values;

	public ExtensiveDomain(Number... values) {
		this(Arrays.asList(values));
	}

	public ExtensiveDomain(List<Number> values) {
		this.values = values;
	}

	public List<Number> getValues() {
		return values;
	}
}
