package cspom.xcsp;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import cspom.compiler.PredicateScanner;
import cspom.variable.CSPOMVariable;

/**
 * This class is used to represent XCSP predicates.
 * 
 * @author vion
 * 
 */
public final class Predicate {

    /**
     * An enumeration to represent various native types for CSP. For now, only
     * integers are supported. Use the static decl(String) method to avoid Java
     * reserved keywords.
     * 
     * @author vion
     * 
     */
    public static enum ValueType {
        /**
         * Standard integer (int) type.
         */
        integer;

        /**
         * Obtain types through this method to avoid Java reserved keywords such
         * as "int".
         * 
         * @param declaration
         *            The XCSP type declaration
         * @return The corresponding ValueType enum element
         */
        public static ValueType decl(final String declaration) {
            if ("int".equals(declaration)) {
                return integer;
            }
            return valueOf(declaration);
        }

    };

    /**
     * Maps parameters to their respective types.
     */
    private final Map<String, ValueType> types;

    /**
     * List of parameters.
     */
    private final List<String> parameters;

    /**
     * Predicate expression.
     */
    private final String expression;

    /**
     * Constructs a Predicate object according to given parameters and
     * expression, in the XCSP format.
     * 
     * @param parametersString
     *            Parameters attribute
     * @param expressionString
     *            Functional content
     */
    public Predicate(final String parametersString,
            final String expressionString) {
        this.parameters = new ArrayList<String>();
        types = new HashMap<String, ValueType>();
        this.expression = expressionString.trim();
        final String[] args = parametersString.trim().split(" +");
        for (int i = 0; i < args.length; i += 2) {
            types.put(args[i + 1], ValueType.decl(args[i]));
            this.parameters.add(args[i + 1]);
        }
    }

    @Override
    public String toString() {
        return "(" + types + "): " + expression;
    }

    /**
     * @return The list of parameters.
     */
    public List<String> getParameters() {
        return parameters;
    }

    /**
     * @return The original expression.
     */
    public String getExpression() {
        return expression;
    }

    /**
     * @param parameter
     *            A parameter
     * @return The type of the given parameter
     */
    public ValueType getTypes(final String parameter) {
        return types.get(parameter);
    }

    @Override
    public boolean equals(final Object object) {
        if (!(object instanceof Predicate)) {
            return false;
        }
        final Predicate predicate = (Predicate) object;
        return predicate.expression.equals(expression);
    }

    @Override
    public int hashCode() {
        return expression.hashCode();
    }

    /**
     * @param name
     *            Name of the sought variable.
     * @param scope
     *            Array of variables to seek.
     * @return the position of the variable having the given name in the given
     *         scope.
     */
    private static int seekVariable(final String name,
            final CSPOMVariable[] scope) {
        for (int i = scope.length; --i >= 0;) {
            if (name.equals(scope[i].getName())) {
                return i;
            }
        }
        return -1;
    }

    /**
     * Apply constraints parameters to the predicate and obtain the modified
     * expression. Parameter names are mapped to variable names or constants.
     * 
     * @param constraintParameters
     *            Constraint parameters
     * @param scope
     *            Scope of the constraint
     * @return The expression obtained by applying the given parameters to the
     *         predicate.
     * @throws ParseException
     *             if the constraint parameters are incompatible with the
     *             predicate parameters.
     * 
     */
    public String applyParameters(final String constraintParameters,
            final CSPOMVariable[] scope) throws ParseException {
        final String[] stringParameters = constraintParameters.trim().split(
                " +");

        if (stringParameters.length != this.parameters.size()) {
            throw new ParseException("Incorrect parameter count", 0);
        }

        String applyied = expression;

        for (int i = 0; i < stringParameters.length; i++) {
            controlParameter(stringParameters[i], scope);
            applyied = applyied.replaceAll(this.parameters.get(i),
                    stringParameters[i]);
        }

        return applyied;
    }

    /**
     * Controls whether the given parameter is valid (either an integer constant
     * or an existing variable).
     * 
     * @param parameter
     *            The parameter.
     * @param scope
     *            An array of variable to validate the parameter against.
     * @throws ParseException
     *             If the given parameter is invalid.
     */
    private void controlParameter(final String parameter,
            final CSPOMVariable[] scope) throws ParseException {
        if (PredicateScanner.INTEGER.matcher(parameter).matches()) {
            return;
        }
        if (PredicateScanner.IDENTIFIER.matcher(parameter).matches()) {
            if (seekVariable(parameter, scope) < 0) {
                throw new ParseException("Could not find variable " + parameter
                        + " in " + Arrays.toString(scope), 0);
            }
            return;
        }
        throw new ParseException("Could not recognize " + parameter, 0);

    }
}
