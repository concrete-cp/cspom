package cspom.constraint;

import javax.script.ScriptException;

import cspom.variable.CSPOMVariable;

public final class GeneralConstraint extends AbstractConstraint {

	public GeneralConstraint(String name, String description, CSPOMVariable... scope) {
		super(name, description, scope);
	}

	public GeneralConstraint(String description, CSPOMVariable... scope) {
		super(description, scope);
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
}
