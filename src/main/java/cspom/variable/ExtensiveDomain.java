package cspom.variable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

/**
 * Implements domains given in extension, that is, a list of arbitrary objects.
 * 
 * @param <T>
 *            Type of objects in the extension.
 * @author vion
 * 
 */
public final class ExtensiveDomain<T> implements CSPOMDomain<T> {

	/**
	 * Number of values displayed by the toString() method.
	 */
	private static final int DISPLAYED_VALUES = 3;

	/**
	 * List of values.
	 */
	private final List<T> values;

	/**
	 * Constructs a new domain containing the given values.
	 * 
	 * @param values
	 *            a set of values of the given type.
	 */
	public ExtensiveDomain(final T... values) {
		this(Arrays.asList(values));
	}

	/**
	 * Constructs a new domain containing the given values.
	 * 
	 * @param values
	 *            a set of values of the given type.
	 */
	public ExtensiveDomain(final List<T> values) {
		this.values = Collections.unmodifiableList(new ArrayList<T>(values));
	}

	@Override
	public List<T> getValues() {
		return values;
	}

	@Override
	public int hashCode() {
		return values.hashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (!(obj instanceof ExtensiveDomain<?>)) {
			return false;
		}
		return values.equals(((ExtensiveDomain<?>) obj).getValues());
	}

	@Override
	public String toString() {
		final Iterator<T> itr = values.iterator();
		if (!itr.hasNext()) {
			return "[]";
		}
		final StringBuilder stb = new StringBuilder();
		stb.append('[');
		int max = DISPLAYED_VALUES;
		for (;;) {
			stb.append(itr.next());
			if (!itr.hasNext()) {
				return stb.append(']').toString();
			}
			if (--max == 0) {
				return stb.append("... (").append(
						values.size() - DISPLAYED_VALUES).append(" more)]")
						.toString();
			}
			stb.append(", ");
		}
	}

	@Override
	public int getSize() {
		return values.size();
	}
}
