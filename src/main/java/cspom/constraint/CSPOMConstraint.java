package cspom.constraint;

import java.util.List;

import cspom.variable.CSPOMVariable;

/**
 * A constraint involves a finite set of variables and is used to define the set
 * of allowed instantiations of these variables. The scope is ordered, and the
 * constraint must provide a way to evaluate an instantiation of its variables
 * (allowed or not). The evaluate method is dedicated to this task.
 * <p>
 * In some cases, one may need a specific permutation of the scope. The
 * standardize method returns a copy of the current constraint with a permuted
 * scope. The new constraints must allow the same instantiations as the original
 * one.
 * <p>
 * The set of allowed instantiations may be managed by a Relation object.
 * <p>
 * CSPOM defines three kinds of constraints: ExtensionConstraint,
 * PredicateConstraint and GlobalConstraint. The helper AbstractConstraint class
 * helps implementing eventual additional constraints.
 * 
 * 
 * @author Julien Vion
 * 
 * @see Relation
 * @see CSPOMVariable
 * @see CSPOM
 */
public interface CSPOMConstraint {

    /**
     * @return the scope of the constraint
     */
    List<CSPOMVariable> getScope();

    /**
     * @return the arity of the constraint. Defined as arity ==
     *         getScope().size()
     */
    int getArity();

    /**
     * Returns the position of the given variable in the constraint's scope.
     * Returns null if the variable is not in the scope.
     * 
     * @param variable
     *            The variable to seek.
     * @return The position of the given variable, or null if could not be
     *         found.
     */
    Integer getPosition(CSPOMVariable variable);

    /**
     * Permutes the scope of the constraint according to the given variables.
     * 
     * @param scope
     *            The new permutation of the scope of the constraint.
     * @return A copy of the constraint with the permuted scope.
     */
    CSPOMConstraint standardize(CSPOMVariable... scope);

    /**
     * @return The description of the constraint.
     */
    String getDescription();
}
