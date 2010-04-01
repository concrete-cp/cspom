package cspom.constraint;

import cspom.variable.CSPOMVariable;

public interface PermutableConstraint extends CSPOMConstraint {
    /**
     * Permutes the scope of the constraint according to the given variables.
     * 
     * @param scope
     *            The new permutation of the scope of the constraint.
     * @return A copy of the constraint with the permuted scope.
     */
    PermutableConstraint standardize(CSPOMVariable... scope);

}
