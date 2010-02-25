package cspom.extension;

import java.util.HashMap;
import java.util.Map;

import cspom.constraint.AbstractConstraint;
import cspom.variable.CSPOMVariable;

/**
 * A class that implements extension constraints, that it constraints defined by
 * a set of allowed or forbidden tuples.
 * 
 * @author vion
 * 
 * @param <T>
 */
public final class ExtensionConstraint<T> extends AbstractConstraint {

    /**
     * The extension defining the tuples the constraint allows or forbids.
     */
    private final Extension<T> relation;

    public ExtensionConstraint(final String name, final Extension<T> relation,
            final CSPOMVariable... scope) {
        super(name, "ext-" + relation, scope);
        this.relation = relation;
    }

    public ExtensionConstraint(final Extension<T> relation,
            final CSPOMVariable... scope) {
        super(null, scope);
        this.relation = relation;
    }

    public Extension<T> getRelation() {
        return relation;
    }

    @Override
    public String toString() {
        return super.toString() + ": " + relation;
    }

    public boolean evaluate(final T[] numbers) {
        return relation.evaluate(numbers);
    }

    public ExtensionConstraint<T> standardize(final CSPOMVariable... scope) {
        assert scope.length == getArity();
        final int[] newPosition = new int[getArity()];
        final Map<CSPOMVariable, CSPOMVariable> newOrder = new HashMap<CSPOMVariable, CSPOMVariable>(
                getArity());

        for (int i = scope.length; --i >= 0;) {
            newOrder.put(getScope().get(i), scope[i]);
        }

        for (int i = getArity(); --i >= 0;) {
            newPosition[i] = getPosition(newOrder.get(getScope().get(i)));
        }

        return new ExtensionConstraint<T>(relation.reverse(newPosition), scope);
    }

}
