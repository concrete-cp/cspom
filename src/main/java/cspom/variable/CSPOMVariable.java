package cspom.variable;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import cspom.constraint.CSPOMConstraint;

/**
 * This class defines and implements CSP variables.
 * 
 * @author vion
 * 
 */
public final class CSPOMVariable {
	/**
	 * Static integer used to generate variable names.
	 */
	private static int unnamed = 0;
	/**
	 * Name of the constraint.
	 */
	private final String name;
	/**
	 * Set of constraints involving this variable.
	 */
	private final Set<CSPOMConstraint> constraints;
	/**
	 * Domain of the variable. Null iff undefined.
	 */
	private CSPOMDomain<?> domain;

	/**
	 * Constructs a new variable with generated name and null domain.
	 */
	public CSPOMVariable() {
		this((CSPOMDomain<?>) null);
	}

	/**
	 * Constructs a new variable with generated name and given domain.
	 * 
	 * @param domain
	 *            Domain of the variable. Null iff undefined.
	 */
	public CSPOMVariable(final CSPOMDomain<?> domain) {
		this(generateName(), domain);
	}

	/**
	 * Constructs a new variable with given name and domain.
	 * 
	 * @param name
	 *            Name of the variable
	 * @param dom
	 *            Domain of the variable. Null iff undefined.
	 */
	public CSPOMVariable(final String name, final CSPOMDomain<?> dom) {
		this.name = name;
		constraints = new HashSet<CSPOMConstraint>();
		this.domain = dom;
	}

	/**
	 * Constructs a new variable with generated name. Domain is defined by lower
	 * and upper bounds.
	 * 
	 * @param <E>
	 *            Type of bounds.
	 * @param lB
	 *            Lower bound of the domain
	 * @param uB
	 *            Upper bound of the domain
	 */
	public <E extends Number & Comparable<E>> CSPOMVariable(final E lB,
			final E uB) {
		this(generateName(), lB, uB);
	}

	/**
	 * Constructs a new variable with given name. Domain is defined by lower and
	 * upper bounds.
	 * 
	 * @param <E>
	 *            Type of the values in the domain
	 * @param name
	 *            Name of the variable
	 * @param lB
	 *            Lower bound of the domain
	 * @param uB
	 *            Upper bound of the domain
	 */
	public <E extends Number & Comparable<E>> CSPOMVariable(final String name,
			final E lB, final E uB) {
		this(name, new Interval<E>(lB, uB));
	}

	/**
	 * Constructs a new variable with given name. Domain is defined by a list of
	 * values.
	 * 
	 * @param <T>
	 *            Type of the values.
	 * @param name
	 *            Name of the variable.
	 * @param values
	 *            List of values defining the domain.
	 */
	public <T> CSPOMVariable(final String name, final List<T> values) {
		this(name, new ExtensiveDomain<T>(values));
	}

	/**
	 * Constructs a new variable with singleton (constant) domain.
	 * 
	 * @param <T>
	 *            The type of the constant.
	 * @param constant
	 *            The unique value of the domain.
	 */
	public <T> CSPOMVariable(final T constant) {
		this(new Constant<T>(constant));
	}

	/**
	 * Constructs a new variable with boolean singleton (constant) domain.
	 * 
	 * @param constant
	 *            The unique value of the domain.
	 */
	public CSPOMVariable(final boolean constant) {
		this(BooleanDomain.valueOf(constant));
	}

	/**
	 * @return The domain of the variable.
	 */
	public CSPOMDomain<?> getDomain() {
		return domain;
	}

	/**
	 * @param dom
	 *            The new domain of the variable.
	 */
	public void setDomain(final CSPOMDomain<?> dom) {
		this.domain = dom;
	}

	/**
	 * @return The name of the variable.
	 */
	public String getName() {
		return name;
	}

	@Override
	public String toString() {
		return name;
	}

	/**
	 * This method is used to register the given constraint in the set of
	 * constraints involving this variable.
	 * 
	 * @param constraint
	 *            The constraint involving the variable.
	 */
	public void registerConstraint(final CSPOMConstraint constraint) {
		if (constraint.getPosition(this) == null) {
			throw new IllegalArgumentException(constraint + " does not imply "
					+ this);
		}
		constraints.add(constraint);
	}

	/**
	 * @return The set of constraints involving this variable.
	 */
	public Set<CSPOMConstraint> getConstraints() {
		return constraints;
	}

	/**
	 * Generates an unique variable name.
	 * 
	 * @return An unique variable name.
	 */
	private static String generateName() {
		return "_" + unnamed++;
	}
}
