package cspom.variable;

import java.util.ArrayList;
import java.util.List;

/**
 * This class implements a constant domain. Obtain instances of Constant through
 * the static method valueOf().
 * 
 * @param <T>
 *            Constant type
 * 
 * @author vion
 * 
 */
public final class Constant<T> implements CSPOMDomain<T> {

	/**
	 * Constant value.
	 */
	private final T value;

	/**
	 * Masked constructor.
	 * 
	 * @param value
	 *            Constant value
	 */
	public Constant(final T value) {
		this.value = value;
	}

	/**
	 * @return The constant value.
	 */
	public T getValue() {
		return value;
	}

	@Override
	public int hashCode() {
		return value.hashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (!(obj instanceof Constant<?>)) {
			return false;
		}
		return ((Constant<?>) obj).value.equals(value);
	}

	@Override
	public String toString() {
		return value.toString();
	}

	@Override
	public List<T> getValues() {
		final List<T> list = new ArrayList<T>(1);
		list.add(value);
		return list;
	}

	@Override
	public int getSize() {
		return 1;
	}

	@Override
	public CSPOMDomain<T> merge(final CSPOMDomain<?> merged) {
		if (merged.getValues().contains(value)) {
			return this;
		}
		throw new IllegalArgumentException("Inconsistent merge");
	}
}
