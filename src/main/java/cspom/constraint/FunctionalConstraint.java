package cspom.constraint;

import javax.script.ScriptException;

import cspom.variable.Variable;

public class FunctionalConstraint extends AbstractConstraint {

	private final Variable result;

	private final Variable[] arguments;

	private final String function;

	public FunctionalConstraint(Variable result, String function,
			Variable... arguments) {
		super(concatenate(result, arguments));
		this.result = result;
		this.arguments = arguments;
		this.function = function;

	}

	private static <T> T[] concatenate(T element, T[] array) {
		T[] result = (T[]) new Object[array.length + 1];
		result[0] = element;
		System.arraycopy(array, 0, result, 1, array.length);
		return result;
	}

	@Override
	public boolean evaluate(Number[] numbers) throws ScriptException {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public Constraint standardize(Variable[] scope) {
		// TODO Auto-generated method stub
		return null;
	}

}
