package cspom.variable;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import cspom.constraint.CSPOMConstraint;

public final class CSPOMVariable {
    private static int noname = 0;
    private final String name;
    private final Set<CSPOMConstraint> constraints;
    private Domain domain;
    private boolean root;

    public CSPOMVariable(final String name, final Domain domain) {
        this.name = name;
        constraints = new HashSet<CSPOMConstraint>();
    }

    public CSPOMVariable(final Domain domain) {
        this(generateName(), domain);
    }

    public CSPOMVariable(final String name, final Number lB, final Number uB) {
        this(name, new Interval(lB, uB));
    }

    public CSPOMVariable(final Number lB, final Number uB) {
        this(generateName(), lB, uB);
    }

    public CSPOMVariable(final String name, final List<Number> values) {
        this(name, new ExtensiveDomain(values));
    }

    public CSPOMVariable(final int constant) {
        this(new Constant(constant));
    }

    public Domain getDomain() {
        return domain;
    }

    public void setDomain(Domain domain) {
        this.domain = domain;
    }

    public String getName() {
        return name;
    }

    public String toString() {
        return name;
    }

    public void registerConstraint(CSPOMConstraint constraint) {
        constraints.add(constraint);
    }

    public Set<CSPOMConstraint> getConstraints() {
        return constraints;
    }

    public void setRoot(boolean root) {
        this.root = root;
    }

    private static String generateName() {
        return "Generated" + noname++;
    }
}
