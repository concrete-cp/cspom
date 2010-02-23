package cspom.constraint;

import java.util.List;

import cspom.variable.CSPOMVariable;

public final class GeneralConstraint extends AbstractConstraint {

    public GeneralConstraint(String name, String description,
            CSPOMVariable... scope) {
        super(name, description, scope);
    }

    public GeneralConstraint(String description, CSPOMVariable... scope) {
        super(description, scope);
    }

    @Override
    public CSPOMConstraint standardize(final CSPOMVariable[] scope) {
        throw new UnsupportedOperationException();
    }

}
