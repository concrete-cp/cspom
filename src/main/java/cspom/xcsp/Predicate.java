package cspom.xcsp;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import cspom.compiler.PredicateScanner;
import cspom.variable.CSPOMVariable;

public final class Predicate {

    public static enum ValueType {
        integer;

        static ValueType decl(String declaration) {
            if ("int".equals(declaration)) {
                return integer;
            }
            return valueOf(declaration);
        }

    };

    private final Map<String, ValueType> types;

    private final List<String> parameters;

    private final String expression;

    // private final static Logger logger = Logger.getLogger(Predicate.class
    // .getSimpleName());

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

    public final List<String> getParameters() {
        return parameters;
    }

    public String getExpression() {
        return expression;
    }

    public final ValueType getTypes(String parameter) {
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

    private static int seekVariable(String string, CSPOMVariable[] scope) {
        for (int i = scope.length; --i >= 0;) {
            if (string.equals(scope[i].getName())) {
                return i;
            }
        }
        return -1;
    }

    public String applyParameters(String parameters, CSPOMVariable[] scope)
            throws ParseException {
        final String[] stringParameters = parameters.trim().split(" +");

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

    private void controlParameter(String string, CSPOMVariable[] scope)
            throws ParseException {
        if (PredicateScanner.INTEGER.matcher(string).matches()) {
            return;
        }
        if (PredicateScanner.IDENTIFIER.matcher(string).matches()) {
            if (seekVariable(string, scope) < 0) {
                throw new ParseException("Could not find variable " + string
                        + " in " + scope, 0);
            }
            return;
        }
        throw new ParseException("Could not recognize " + string, 0);

    }
}
