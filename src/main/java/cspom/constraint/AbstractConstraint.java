package cspom.constraint;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;

import com.google.common.base.Function;
import com.google.common.base.Preconditions;
import com.google.common.base.Predicate;
import com.google.common.base.Predicates;

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

    private final String parameters;

    private int hashCode = 0;

    public AbstractConstraint(final String name, final String description,
            final String parameters, final List<CSPOMVariable> scope) {
        Preconditions.checkArgument(!scope.isEmpty(),
                "A constraint must imply at least one variable");
        this.scope = scope;
        this.name = name;
        this.description = description;
        this.parameters = parameters;
        arity = scope.size();
        positions = new HashMap<CSPOMVariable, Integer>(arity);
        for (final ListIterator<CSPOMVariable> itr = scope.listIterator(); itr
                .hasNext();) {
            final int index = itr.nextIndex();
            positions.put(itr.next(), index);
        }
        computeHashCode();
    }

    public AbstractConstraint(final String name, final String description,
            final String parameters, final CSPOMVariable... scope) {
        this(name, description, parameters, Arrays.asList(scope));
    }

    public AbstractConstraint(final String description,
            final String parameters, final List<CSPOMVariable> scope) {
        this(null, description, parameters, scope);
    }

    public AbstractConstraint(final String description,
            final String parameters, final CSPOMVariable... scope) {
        this(null, description, parameters, scope);
    }

    @Override
    public final List<CSPOMVariable> getScope() {
        return scope;
    }

    @Override
    public final int getArity() {
        return arity;
    }

    @Override
    public final Integer getPosition(final CSPOMVariable variable) {
        return positions.get(variable);
    }

    @Override
    public final boolean involves(final CSPOMVariable variable) {
        return positions.containsKey(variable);
    }

    @Override
    public final boolean involvesAll(final Collection<CSPOMVariable> variables) {
        return positions.keySet().containsAll(variables);
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
    public int hashCode() {
        return hashCode;
    }

    private void computeHashCode() {
        hashCode = scope.hashCode() + 31 * getDescription().hashCode();
    }

    @Override
    public boolean equals(final Object object) {
        if (!(object instanceof AbstractConstraint)) {
            return false;
        }
        final AbstractConstraint constraint = (AbstractConstraint) object;
        return scope.equals(constraint.scope)
                && description.equals(constraint.description);
    }

    @Override
    public final Iterator<CSPOMVariable> iterator() {
        return scope.iterator();
    }

    @Override
    public void replaceVar(final CSPOMVariable merged, final CSPOMVariable var) {
        int pos = scope.indexOf(merged);
        if (pos < 0) {
            throw new IllegalArgumentException(merged + " not in scope");
        }
        do {
            scope.set(pos, var);
            positions.remove(merged);
            positions.put(var, pos);
            pos = scope.indexOf(merged);
        } while (pos >= 0);
        computeHashCode();
    }

    public final String getParameters() {
        return parameters;
    }

    public static final Function<CSPOMConstraint, String> CONSTRAINT_DESCRIPTION = new Function<CSPOMConstraint, String>() {
        @Override
        public String apply(final CSPOMConstraint input) {
            return input.toString();
        }
    };

    public static Predicate<CSPOMConstraint> matchesDescription(
            final String description) {
        return Predicates.compose(Predicates.equalTo(description),
                CONSTRAINT_DESCRIPTION);
    }
}
