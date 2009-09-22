package cspom.xcsp;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import cspom.variable.Variable;

public class Predicate {

	private final Map<String, Type> types;

	private final List<String> parameters;

	private final String expression;

	private final static Logger logger = Logger.getLogger(Predicate.class
			.getSimpleName());

	public Predicate(final String parameters, final String expression) {
		this.parameters = new ArrayList<String>();
		types = new HashMap<String, Type>();
		this.expression = expression.trim();
		final String[] args = parameters.trim().split(" +");
		for (int i = 0; i < args.length; i += 2) {
			types.put(args[i + 1], Type.parse(args[i]));
			this.parameters.add(args[i + 1]);
		}
	}

	public String toString() {
		return super.toString() + " (" + types + "): " + expression;
	}

	public final List<String> getParameters() {
		return parameters;
	}

	public String getExpression() {
		return expression;
	}

	public final Map<String, Type> getTypes() {
		return types;
	}

	public boolean equals(final Object object) {
		if (!(object instanceof Predicate)) {
			return false;
		}
		final Predicate predicate = (Predicate) object;
		return predicate.expression.equals(expression);
	}

	public int hashCode() {
		return expression.hashCode();
	}

	public enum Type {
		INTEGER, UNKNOWN;

		public static Type parse(final String type) {
			if ("int".equals(type)) {
				return INTEGER;
			}
			return UNKNOWN;
		}
	}
	
	 private static List<Parameter> parseParameters(
	            final String allParameters, final Predicate predicate,
	            final Variable[] scope) {
	        final List<Parameter> parameters = new ArrayList<Parameter>();

	        final String[] stringParameters = allParameters.trim().split(
	                " +");

	        for (int i = 0; i < stringParameters.length; i++) {
	            final String parameterName = predicate.getParameters()
	                    .get(i);

	            if (CONSTANT_PATTERN.matcher(stringParameters[i])
	                    .matches()) {
	                parameters.add(new ConstantParameter(parameterName,
	                        Integer.parseInt(stringParameters[i])));
	            } else {
	                final int variable = seekVariable(
	                        stringParameters[i], scope);

	                if (variable >= 0) {
	                    parameters.add(new VariableParameter(
	                            parameterName, scope[variable]));
	                } else {
	                    throw new IllegalArgumentException(
	                            "Could not find variable "
	                                    + stringParameters[i] + " "
	                                    + scope);
	                }
	            }
	        }
	        return parameters;
	    }
}
