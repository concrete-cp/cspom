/*
 * Created on 14 janv. 08
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package cspom;

import java.security.InvalidParameterException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import cspom.constraint.Constraint;
import cspom.variable.Variable;

public class Scope {
	private final Collection<Constraint> constraints;

	private final List<Variable> scope;
	
	private final int arity;
	
	public Scope(final Constraint constraint) {
		this.scope = constraint.getScope();
		this.arity = constraint.getArity();
		constraints = new ArrayList<Constraint>();
		constraints.add(constraint);
	}
	
	public List<Variable> getScope() {
		return scope;
	}
	
	public static Scope findScope(final Collection<Variable> scope, final Collection<Scope> scopes) {
		for (Scope s: scopes) {
			if (s.isSameScope(scope)) {
				return s;
			}
		}
		return null;
	}
	
	public int getArity() {
		return arity;
	}
	
	public boolean isSameScope(final Collection<Variable> scope) {
		if (scope.size() != arity) {
			return false ;
		}
		for (Variable variable: scope) {
			if (!this.scope.contains(variable)) {
				return false ;
			}
		}
		return true;
	}
	
	public void addConstraint(final Constraint constraint) {
		if (!isSameScope(constraint.getScope())) {
			throw new InvalidParameterException("Bad scope for " + constraint + " in " + this);
		}
		constraints.add(constraint);
	}
	
	public Collection<Constraint> getConstraints() {
		return constraints;
	}
	
	public Collection<Constraint> getStandardizedConstraints() {
		final Collection<Constraint> standardized = new ArrayList<Constraint>(constraints.size());
		for (Constraint c: constraints) {
			if (c.getScope().equals(scope)) {
				standardized.add(c);
			} else {
				standardized.add(c.standardize(scope));
			}
		}
		return standardized;
	}
	
	public String toString() {
		return scope + ", " + constraints.size() + " constraints";
	}
}