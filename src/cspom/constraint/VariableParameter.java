package cspom.constraint;

import cspom.variable.Variable;

public class VariableParameter implements Parameter {

	private final Variable variable;
	private final String name ;
//	private Number current;
	public final static String PATTERN = "\\ *[a-zA-Z_][a-zA-Z_0-9]*\\ *";

	public VariableParameter(final String name, final Variable variable) {
		this.variable = variable;
		this.name = name;
	}

	public String toString() {
		return name + "=" + variable.toString();
	}

//	public void setNumber(final Number value) {
//		current = value;
//	}
//
//	public Number getNumber() {
//		return current;
//	}
	
	public Variable getVariable() {
		return variable;
	}
	
	public String getName() {
		return name ;
	}
	
}
