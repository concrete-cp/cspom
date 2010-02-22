package cspom.variable;

import java.util.HashMap;
import java.util.Map;

/**
 * Class for easy use of enum-type domains, that is, domains that can take
 * values in the various elements of a given enum type.
 * 
 * @param <T>
 *            The enum type
 * 
 * @author vion
 * 
 */
public final class EnumDomain<T extends Enum<T>> extends ExtensiveDomain<T> {

    /**
     * Map used to retrieve singleton instances of EnumDomain.
     */
    private static final Map<Class<? extends Enum<?>>, EnumDomain<? extends Enum<?>>> DOMAINS = new HashMap<Class<? extends Enum<?>>, EnumDomain<? extends Enum<?>>>();

    /**
     * Factory method to retrieve instances of EnumDomain.
     * 
     * @param <T>
     *            The enum type
     * @param clazz
     *            Class of the enum type
     * @return An instance of EnumDomain corresponding to the given enum type.
     */
    public static synchronized <T extends Enum<T>> EnumDomain<T> getEnumDomain(
            final Class<T> clazz) {
        EnumDomain<T> domain = (EnumDomain<T>) DOMAINS.get(clazz);
        if (domain == null) {
            domain = new EnumDomain<T>(clazz);
            DOMAINS.put(clazz, domain);
        }
        return domain;

    }

    /**
     * Hidden constructor. Please use the factory method.
     * 
     * @param clazz
     *            Class of the enum type
     */
    private EnumDomain(final Class<T> clazz) {
        super(clazz.getEnumConstants());
    }
}
