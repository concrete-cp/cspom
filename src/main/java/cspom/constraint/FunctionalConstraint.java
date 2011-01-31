package cspom.constraint;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.ListIterator;
import java.util.logging.Logger;

import javax.script.ScriptException;

import com.google.common.base.Joiner;
import com.google.common.base.Preconditions;
import com.google.common.base.Predicate;
import com.google.common.base.Predicates;
import com.google.common.collect.Iterables;

import cspom.Evaluator;
import cspom.variable.CSPOMVariable;

public final class FunctionalConstraint extends AbstractConstraint {

    private static final Logger LOGGER = Logger
            .getLogger(FunctionalConstraint.class.getName());

    private CSPOMVariable result;

    private List<CSPOMVariable> arguments;

    public FunctionalConstraint(final CSPOMVariable result,
            final String function, final String parameters,
            final List<CSPOMVariable> arguments) {
        super(function, parameters, concat(result, arguments));
        Preconditions.checkArgument(!arguments.isEmpty(),
                "Must have at least one argument");

        this.result = result;
        this.arguments = arguments;

    }

    private static List<CSPOMVariable> concat(final CSPOMVariable var,
            final Collection<CSPOMVariable> arguments) {
        final List<CSPOMVariable> array = new ArrayList<CSPOMVariable>(
                arguments.size() + 1);
        array.add(var);
        array.addAll(arguments);
        return array;
    }

    public FunctionalConstraint(final CSPOMVariable result,
            final String function, final String parameters,
            final CSPOMVariable... arguments) {
        this(result, function, parameters, Arrays.asList(arguments));
    }

    public CSPOMVariable getResultVariable() {
        return result;
    }

    public List<CSPOMVariable> getArguments() {
        return arguments;
    }

    @Override
    public String toString() {
        final StringBuilder stb = new StringBuilder();
        stb.append(result).append(" = ").append(getDescription());
        if (getParameters() != null) {
            stb.append('{').append(getParameters()).append('}');
        }

        stb.append('(');
        Joiner.on(", ").appendTo(stb, arguments);
        return stb.append(')').toString();
    }

    @Override
    public void replaceVar(final CSPOMVariable merged, final CSPOMVariable var) {
        super.replaceVar(merged, var);
        if (result == merged) {
            result = var;
        }
        for (final ListIterator<CSPOMVariable> itr = arguments.listIterator(); itr
                .hasNext();) {
            if (itr.next() == merged) {
                itr.set(var);
            }
        }
    }

    @Override
    public boolean evaluate(final Object[] tuple) {
        final StringBuilder stb = new StringBuilder();
        stb.append(tuple[0]).append(" == ").append(getDescription())
                .append('(');
        Joiner.on(", ").appendTo(stb, Iterables.skip(Arrays.asList(tuple), 1));

        if (getParameters() != null) {
            stb.append(", ").append(getParameters());
        }

        try {
            return Evaluator.evaluate(stb.append(")").toString());
        } catch (ScriptException e) {
            LOGGER.throwing(FunctionalConstraint.class.getName(), "evaluate", e);
            throw new IllegalStateException(e);
        }

    }

    public static Predicate<CSPOMConstraint> matchesDescription(
            final String description) {
        return Predicates.and(
                AbstractConstraint.matchesDescription(description),
                Predicates.instanceOf(FunctionalConstraint.class));
    }
}
