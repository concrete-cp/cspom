package cspom.constraint;

import java.util.Arrays;

import javax.script.ScriptException;

import cspom.variable.CSPOMVariable;

public class FunctionalConstraint extends AbstractConstraint {

	private final CSPOMVariable result;

	private final CSPOMVariable[] arguments;

	public FunctionalConstraint(CSPOMVariable result, String function,
			CSPOMVariable... arguments) {
		super(function, concatenate(result, arguments));
		this.result = result;
		this.arguments = arguments;

	}

	private static CSPOMVariable[] concatenate(CSPOMVariable element, CSPOMVariable[] array) {
		CSPOMVariable[] result = new CSPOMVariable[array.length + 1];
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
	public CSPOMConstraint standardize(CSPOMVariable[] scope) {
		// TODO Auto-generated method stub
		return null;
	}

	public CSPOMVariable getResultVariable() {
		return result;
	}

	public CSPOMVariable[] getArguments() {
		return arguments;
	}

	public String toString() {
		return result + " = " + getDescription() + Arrays.toString(arguments);
	}

}
