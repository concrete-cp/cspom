package cspom.constraint;

import java.util.Arrays;

import javax.script.ScriptException;

import cspom.variable.Variable;

public class FunctionalConstraint extends AbstractConstraint {

	private final Variable result;

	private final Variable[] arguments;

	public FunctionalConstraint(Variable result, String function,
			Variable... arguments) {
		super(function, concatenate(result, arguments));
		this.result = result;
		this.arguments = arguments;

	}

	private static Variable[] concatenate(Variable element, Variable[] array) {
		Variable[] result = new Variable[array.length + 1];
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

	public Variable getResultVariable() {
		return result;
	}

	public Variable[] getArguments() {
		return arguments;
	}

	public String toString() {
		return result + " = " + getDescription() + Arrays.toString(arguments);
	}

}
