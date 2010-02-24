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
public final class Interval<T extends Number & Comparable<T>> implements
        Domain<T> {

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

    public static <E extends Number & Comparable<E>> Interval<E> valueOf(
            String interval) {
        final String[] fromto = interval.trim().split("\\.\\.");
        final E lb = numValueOf(fromto[0]);
        final E ub = numValueOf(fromto[1]);
        return new Interval<E>(lb, ub);
    }

    public static <E extends Number & Comparable<E>> E numValueOf(String value) {
        try {
            return (E) Integer.valueOf(value);
        } catch (NumberFormatException e) {
            //
        }
        try {
            return (E) Long.valueOf(value);
        } catch (NumberFormatException e) {
            //
        }
        try {
            return (E) Float.valueOf(value);
        } catch (NumberFormatException e) {
            //
        }

        return (E) Double.valueOf(value);
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
    public int hashCode() {
        return 961 + 31 * lb.hashCode() + ub.hashCode();
    }

    @Override
    public boolean equals(final Object obj) {
        if (!(obj instanceof Interval<?>)) {
            return false;
        }
        final Interval<?> itv = (Interval<?>) obj;
        return lb.equals(itv.getLb()) && ub.equals(itv.getUb());
    }

    @Override
    public List<T> getValues() {
        if (Integer.class.isInstance(lb)) {
            final List<Integer> list = new ArrayList<Integer>();
            final int intUb = this.ub.intValue();
            for (int i = lb.intValue(); i <= intUb; i++) {
                list.add(i);
            }

            return List.class.cast(list);
        }
        if (Long.class.isInstance(lb)) {
            final List<Long> list = new ArrayList<Long>();
            final long longUb = this.ub.longValue();
            for (long i = lb.longValue(); i <= longUb; i++) {
                list.add(i);
            }
            return List.class.cast(list);
        }
        throw new IllegalArgumentException(
                "Cannot obtain list of values from an interval of "
                        + lb.getClass());
    }
}
