package cspom.variable;

import java.util.ArrayList;
import java.util.List;

/**
 * This class implements a domain given as an interval (lower bound & upper
 * bound).
 * 
 * @param <T>
 *            Type of bounds
 * 
 * @author vion
 */
public final class IntInterval implements CSPOMDomain<Integer> {

	/**
	 * Base number used to compute hash codes.
	 */
	private static final int HASH_BASE = 31;

	/**
	 * Lower bound.
	 */
	private final int lb;
	/**
	 * Upper bound.
	 */
	private final int ub;

	/**
	 * Constructs a new interval domain according to given lower and upper
	 * bounds. Both bound must be of the same type, and lower bound must be <=
	 * to upper bound (bounds types must thus implement the Comparable
	 * interface).
	 * 
	 * @throws IllegalArgumentException
	 *             if given bounds are not legal.
	 * 
	 * @param lb
	 *            lower bound
	 * @param ub
	 *            upper bound
	 */
	public IntInterval(final int lb, final int ub) {
		if (lb > ub) {
			throw new IllegalArgumentException(
					"Lower bound must be inferior or equal to upper bound");
		}
		this.lb = lb;
		this.ub = ub;

	}

	/**
	 * @param interval
	 *            a String containing the interval representation to be parsed.
	 *            The format of the interval is "a..b".
	 * @return an Interval instance corresponding to the parsed interval.
	 *         Currently returns either an Interval of Integers or Doubles.
	 * 
	 * @throws NumberFormatException
	 *             if the interval could not be parsed.
	 */
	public static IntInterval valueOf(final String interval) {
		final String[] fromto = interval.trim().split("\\.\\.");
		if (fromto.length != 2) {
			throw new NumberFormatException("Interval format must be a..b");
		}
		final int lb = Integer.parseInt(fromto[0]);
		final int ub = Integer.parseInt(fromto[1]);
		return new IntInterval(lb, ub);
	}

	/**
	 * @return the lower bound
	 */
	public int getLb() {
		return lb;
	}

	/**
	 * @return the upper bound
	 */
	public int getUb() {
		return ub;
	}

	@Override
	public String toString() {
		return "[" + lb + ".." + ub + "]";
	}

	@Override
	public int hashCode() {
		return HASH_BASE * lb + ub;
	}

	@Override
	public boolean equals(final Object obj) {
		if (!(obj instanceof IntInterval)) {
			return false;
		}
		final IntInterval itv = (IntInterval) obj;
		return lb == itv.lb && ub == itv.ub;
	}

	@Override
	public List<Integer> getValues() {

		final List<Integer> list = new ArrayList<Integer>();

		for (int i = lb; i <= ub; i++) {
			list.add(i);
		}

		return list;

	}

	@Override
	public int getSize() {

		return 1 + ub - lb;

	}

	@Override
	public CSPOMDomain<Integer> merge(final CSPOMDomain<?> merged) {
		if (merged instanceof IntInterval) {
			final IntInterval mergeInt = (IntInterval) merged;
			final int newlb = Math.max(lb, mergeInt.getLb());
			final int newub = Math.min(ub, mergeInt.getUb());
			return new IntInterval(newlb, newub);
		}

		return (CSPOMDomain<Integer>) merged.merge(this);
	}
}
