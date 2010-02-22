package cspom.variable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * This class implements a constant domain. Obtain instances of Constant through
 * the static method valueOf().
 * 
 * @author vion
 * 
 */
public final class Constant<T extends Number> implements Domain {

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
    public Object clone() throws CloneNotSupportedException {
        throw new CloneNotSupportedException();
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
}
