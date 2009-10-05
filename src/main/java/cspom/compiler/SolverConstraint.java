package cspom.compiler;

import java.util.List;

import cspom.variable.UnknownDomain.Type;

public interface SolverConstraint {

	String getPattern();
	
	List<Type> getTypes(int nbArgs);
	
	void generateDomain(int position);
	
}
