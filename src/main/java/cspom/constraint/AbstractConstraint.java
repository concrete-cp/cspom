package cspom.constraint;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import cspom.variable.CSPOMVariable;

/**
 * This class provides a skeletal implementation of the <tt>CSPOMConstraint</tt>
 * interface, to minimize the effort required to implement this interface.
 * 
 * To implement a constraint, the programmer needs only to extend this class and
 * provide implementations for the iterator and size methods. (The iterator
 * returned by the iterator method must implement hasNext and next.)
 * 
 * 
 * 
 * @author vion
 * 
 */
public abstract class AbstractConstraint implements CSPOMConstraint {

	private final String name;

	private final List<CSPOMVariable> scope;

	private final int arity;

	private final Map<CSPOMVariable, Integer> positions;

	private final String description;

	public AbstractConstraint(final String description,
			final CSPOMVariable... scope) {
		this(null, description, scope);
	}

	public AbstractConstraint(final String name, final String description,
			final CSPOMVariable... scope) {
		this.scope = Collections.unmodifiableList(Arrays.asList(scope));
		this.name = name;
		this.description = description;
		arity = scope.length;
		positions = new HashMap<CSPOMVariable, Integer>(arity);
		for (int i = arity; --i >= 0;) {
			positions.put(scope[i], i);
			scope[i].registerConstraint(this);
		}
	}

	public final List<CSPOMVariable> getScope() {
		return scope;
	}

	@Override
	public String toString() {
		return "(" + scope + ")";
	}

	public final int getArity() {
		return arity;
	}

	public final Integer getPosition(final CSPOMVariable variable) {
		return positions.get(variable);
	}

	public int hashCode() {
		return scope.hashCode() + 31 * getDescription().hashCode();
	}

	@Override
	public final String getDescription() {
		return description;
	}

	@Override
	public final String getName() {
		return name;
	}

	@Override
	public final CSPOMVariable getVariable(final int position) {
		return scope.get(position);
	}

	@Override
	public boolean equals(Object object) {
		if (!(object instanceof AbstractConstraint)) {
			return false;
		}
		final AbstractConstraint constraint = (AbstractConstraint) object;
		return scope.equals(constraint.getScope())
				&& description.equals(constraint.description);
	}
}
