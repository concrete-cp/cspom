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
    private Domain domain;

    /**
     * Constructs a new variable with generated name and null domain.
     */
    public CSPOMVariable() {
        this((Domain) null);
    }

    /**
     * Constructs a new variable with generated name and given domain.
     * 
     * @param domain
     *            Domain of the variable. Null iff undefined.
     */
    public CSPOMVariable(final Domain domain) {
        this(generateName(), domain);
    }

    /**
     * Constructs a new variable with given name and domain.
     * 
     * @param name
     *            Name of the variable
     * @param domain
     *            Domain of the variable. Null iff undefined.
     */
    public CSPOMVariable(final String name, final Domain domain) {
        this.name = name;
        constraints = new HashSet<CSPOMConstraint>();
        this.domain = domain;
    }

    /**
     * Constructs a new variable with generated name. Domain is defined by lower
     * and upper bounds.
     * 
     * @param lB
     *            Lower bound of the domain
     * @param uB
     *            Upper bound of the domain
     */
    public CSPOMVariable(final Number lB, final Number uB) {
        this(generateName(), lB, uB);
    }

    /**
     * Constructs a new variable with given name. Domain is defined by lower and
     * upper bounds.
     * 
     * @param name
     *            Name of the variable
     * @param lB
     *            Lower bound of the domain
     * @param uB
     *            Upper bound of the domain
     */
    public CSPOMVariable(final String name, final Number lB, final Number uB) {
        this(name, new Interval(lB, uB));
    }

    /**
     * Constructs a new variable with given name. Domain is defined by a list of
     * numeric values.
     * 
     * @param name
     *            Name of the variable.
     * @param values
     *            List of values defining the domain.
     */
    public CSPOMVariable(final String name, final List<Number> values) {
        this(name, new ExtensiveDomain(values));
    }

    /**
     * Constructs a new variable with singleton (constant) domain.
     * 
     * @param constant
     *            The unique value of the domain.
     */
    public CSPOMVariable(final Number constant) {
        this(Constant.valueOf(constant));
    }

    /**
     * @return The domain of the variable.
     */
    public Domain getDomain() {
        return domain;
    }

    /**
     * @param domain
     *            The new domain of the variable.
     */
    public void setDomain(final Domain domain) {
        this.domain = domain;
    }

    /**
     * @return The name of the variable.
     */
    public String getName() {
        return name;
    }

    @Override
    public String toString() {
        return name + " [" + domain + "]";
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
