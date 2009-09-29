package cspom.constraint;

import java.util.Arrays;

import javax.script.ScriptException;

import cspom.variable.Variable;

public class GeneralConstraint extends AbstractConstraint {

	public GeneralConstraint(String name, String description, Variable... scope) {
		super(name, description, scope);
	}

	public GeneralConstraint(String description, Variable... scope) {
		super(description, scope);
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
