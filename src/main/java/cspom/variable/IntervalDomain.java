package cspom.variable;

import java.util.ArrayList;
import java.util.List;

public class IntervalDomain implements Domain {

	public final int min, max;
	public final String name;
	public final int nbValues;

	public IntervalDomain(final String name, final int min, final int max) {
		this.min = min;
		this.max = max;
		this.name = name;
		nbValues = max-min+1;
	}

	public List<Number> getValues() {
		final List<Number> values = new ArrayList<Number>();
		for (int i = min; i <= max; i++) {
			values.add(i);
		}
		return values;
	}

	public String getName() {
		return name;
	}

	public int getNbValues() {
		return nbValues;
	}
	
	@Override
	public String toString() {
		return name + ": [" + min + ".." + max + ']';
	}

}
