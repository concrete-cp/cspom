package cspom.constraint;

import javax.script.ScriptException;

import cspom.variable.Variable;

public class GeneralConstraint extends AbstractConstraint {
	private String description;

	public GeneralConstraint(String name, String description, Variable... scope) {
		super(name, scope);
		this.description = description;
	}

	public GeneralConstraint(String description, Variable... scope) {
		super(scope);
		this.description = description;
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
