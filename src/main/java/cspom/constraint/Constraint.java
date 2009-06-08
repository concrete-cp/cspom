package cspom.constraint;

import javax.script.ScriptException;

import cspom.Problem;
import cspom.Relation;
import cspom.variable.Variable;

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
 * @see Variable
 * @see Problem
 */
public interface Constraint {

    /**
     * @return the scope of the constraint
     */
    Variable[] getScope();

    /**
     * @return the name of the constraint
     */
    String getName();

    /**
     * @return the arity of the constraint. Defined as arity ==
     *         getScope().size()
     */
    int getArity();

    /**
     * Evaluates a given instantiation.
     * 
     * @param numbers
     *            the instantiation to evaluate. For all i,
     *            getScope()[i].contains(numbers[i]).
     * @return true iff the constraint allows the instantiation
     * @throws ScriptException
     */
    boolean evaluate(Number[] numbers) throws ScriptException;

    int getPosition(Variable variable);

    Constraint standardize(Variable[] scope);

    /**
     * @return the signature of the constraints. The signature is calculated by
     *         summing the hashcodes of the domain of the involved variables.
     */
    DomainSignature signature();

    Relation getRelation();
}
