package cspom.constraint;

import java.util.regex.Pattern;

import cspom.variable.Variable;

public class VariableParameter implements Parameter {

	private final Variable variable;
	private final String name ;
//	private Number current;
	public final static String PATTERN = "\\ *[a-zA-Z_][a-zA-Z_0-9]*\\ *";

	public VariableParameter(String name, Variable variable) {
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
	
	public static void main(String[] args) {
		Pattern pattern = Pattern.compile(PATTERN);
		System.out.println(pattern.matcher(args[0]).matches());
	}
	

}
