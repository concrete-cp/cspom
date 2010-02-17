package cspom.variable;

import java.util.HashMap;
import java.util.Map;

/**
 * This class implements a constant domain. Obtain instances of Constant through
 * the static method valueOf().
 * 
 * @author vion
 * 
 */
public final class Constant implements Domain {

    /**
     * Map used to store singleton Constants.
     */
    private static final Map<Number, Constant> CONSTANTS = new HashMap<Number, Constant>();

    /**
     * @param value
     *            The value of the constant.
     * @return A (singleton) Constant instance representing the given numeric
     *         value.
     */
    public static synchronized Constant valueOf(final Number value) {
        final Constant constant = CONSTANTS.get(value);
        if (constant == null) {
            final Constant newConstant = new Constant(value);
            CONSTANTS.put(value, newConstant);
            return newConstant;
        }
        return constant;
    }

    /**
     * Constant value.
     */
    private final Number value;

    /**
     * Masked constructor.
     * 
     * @param value
     *            Constant value
     */
    private Constant(final Number value) {
        this.value = value;
    }

    /**
     * @return The constant value.
     */
    public Number getValue() {
        return value;
    }

    @Override
    public Object clone() throws CloneNotSupportedException {
        throw new CloneNotSupportedException();
    }

    @Override
    public String toString() {
        return value.toString();
    }

}
