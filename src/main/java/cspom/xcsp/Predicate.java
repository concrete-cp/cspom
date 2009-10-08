package cspom.xcsp;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import cspom.compiler.PredicateScanner;
import cspom.variable.CSPOMVariable;

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
