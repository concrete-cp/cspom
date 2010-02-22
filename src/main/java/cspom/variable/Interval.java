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
public final class Interval<T extends Number & Comparable<T>> implements Domain {

    /**
     * Lower bound.
     */
    private final T lb;
    /**
     * Upper bound.
     */
    private final T ub;

    /**
     * Constructs a new interval domain according to given lower and upper
     * bounds. Both bound must be of the same type, and lower bound must be <=
     * to upper bound.
     * 
     * @throws IllegalArgumentException
     *             if given bounds are not legal.
     * 
     * @param lb
     *            lower bound
     * @param ub
     *            upper bound
     */
    public Interval(final T lb, final T ub) {
        if (!lb.getClass().equals(ub.getClass())) {
            throw new IllegalArgumentException(
                    "Both bounds must be of the same type");
        }
        if (lb.compareTo(ub) > 0) {
            throw new IllegalArgumentException(
                    "Lower bound must be inferior or equal to upper bound");
        }
        this.lb = lb;
        this.ub = ub;

    }

    /**
     * @return the lower bound
     */
    public T getLb() {
        return lb;
    }

    /**
     * @return the upper bound
     */
    public T getUb() {
        return ub;
    }

    @Override
    public String toString() {
        return "[" + lb + ".." + ub + "]";
    }

    @Override
    public List<T> getValues() {
        if (lb instanceof Integer) {
            final List<Integer> list = new ArrayList<Integer>();
            final int intUb = (Integer) this.ub;
            for (int i = (Integer) lb; i <= intUb; i++) {
                list.add(i);
            }

            return (List<T>) list;
        }
        if (lb instanceof Long) {
            final List<Long> list = new ArrayList<Long>();
            final long longUb = (Long) this.ub;
            for (long i = (Long) lb; i <= longUb; i++) {
                list.add(i);
            }

            return (List<T>) list;
        }
        throw new IllegalArgumentException(
                "Cannot obtain list of values from an interval of "
                        + lb.getClass());
    }
}
