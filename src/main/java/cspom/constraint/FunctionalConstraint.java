package cspom.constraint;

import javax.script.ScriptException;

import cspom.Evaluator;
import cspom.variable.CSPOMVariable;

public final class FunctionalConstraint<T> extends AbstractConstraint<T> {

	private CSPOMVariable result;

	private CSPOMVariable[] arguments;

	public FunctionalConstraint(final CSPOMVariable result,
			final String function, final Object parameters,
			final CSPOMVariable[] arguments) {
		super(function, parameters, concatenate(result, arguments));
		if (arguments.length == 0) {
			throw new IllegalArgumentException(
					"Must have at least one argument");
		}
		this.result = result;
		this.arguments = arguments;

	}

	private static CSPOMVariable[] concatenate(final CSPOMVariable element,
			final CSPOMVariable[] array) {
		CSPOMVariable[] result = new CSPOMVariable[array.length + 1];
		result[0] = element;
		System.arraycopy(array, 0, result, 1, array.length);
		return result;
	}

	public CSPOMVariable getResultVariable() {
		return result;
	}

	public CSPOMVariable[] getArguments() {
		return arguments;
	}

	@Override
	public String toString() {
		final StringBuilder stb = new StringBuilder();
		stb.append(result).append(" = ").append(getDescription()).append('{')
				.append(getParameters()).append('}');

		int iMax = arguments.length - 1;

		stb.append('(');
		for (int i = 0;; i++) {
			stb.append(arguments[i]);
			if (i == iMax) {
				return stb.append(')').toString();
			}
			stb.append(", ");
		}
	}

	@Override
	public void replaceVar(final CSPOMVariable merged, final CSPOMVariable var) {
		super.replaceVar(merged, var);
		if (result == merged) {
			result = var;
		}
		for (int i = arguments.length; --i >= 0;) {
			if (arguments[i] == merged) {
				arguments[i] = var;
			}
		}
	}

	@Override
	public boolean evaluate(final T[] tuple) {
		final StringBuilder stb = new StringBuilder();
		stb.append(tuple[0]).append(" == ").append(getDescription());

		int iMax = arguments.length - 1;

		stb.append('(');
		for (int i = 0;; i++) {
			stb.append(tuple[i + 1]);
			if (i == iMax) {
				try {
					return Evaluator.evaluate(stb.append(')').toString());
				} catch (ScriptException e) {
					throw new IllegalStateException(e);
				}
			}
			stb.append(", ");
		}
	}

}
