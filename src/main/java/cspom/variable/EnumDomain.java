package cspom.variable;

import java.util.HashMap;
import java.util.List;
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
public final class EnumDomain<T extends Enum<T>> implements CSPOMDomain<T> {

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
	 * Underlying extensive domain. No specialization here to keep objects
	 * immutable (ExtensiveDomain is final).
	 */
	private final ExtensiveDomain<T> underlying;

	/**
	 * Hidden constructor. Please use the factory method.
	 * 
	 * @param clazz
	 *            Class of the enum type
	 */
	private EnumDomain(final Class<T> clazz) {
		underlying = new ExtensiveDomain<T>(clazz.getEnumConstants());
	}

	@Override
	public List<T> getValues() {
		return underlying.getValues();
	}

	@Override
	public int hashCode() {
		return underlying.hashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (!(obj instanceof EnumDomain<?>)) {
			return false;
		}
		return underlying.equals(((EnumDomain<?>) obj).underlying);
	}

	@Override
	public String toString() {
		return underlying.toString();
	}

	@Override
	public int getSize() {
		return underlying.getSize();
	}
}
