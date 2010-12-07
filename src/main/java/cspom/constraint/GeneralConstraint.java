package cspom.constraint;

import javax.script.ScriptException;

import cspom.Evaluator;
import cspom.variable.CSPOMVariable;

public final class GeneralConstraint extends AbstractConstraint {

    public GeneralConstraint(final String name, final String description,
            final String parameters, final CSPOMVariable... scope) {
        super(name, description, parameters, scope);
        if (scope.length == 0) {
            throw new IllegalArgumentException(
                    "A constraint must imply at least one variable");
        }
    }

    public GeneralConstraint(final String description, final String parameters,
            final CSPOMVariable... scope) {
        super(description, parameters, scope);
    }

    public String toString() {
        final StringBuilder stb = new StringBuilder();
        stb.append(getDescription());
        if (getParameters() != null) {
            stb.append('{').append(getParameters()).append('}');
        }

        int iMax = getScope().size() - 1;
        stb.append('(');
        for (int i = 0;; i++) {
            stb.append(getVariable(i));
            if (i == iMax) {
                return stb.append(')').toString();
            }
            stb.append(", ");
        }
    }

    @Override
    public boolean evaluate(final Object[] tuple) {
        final StringBuilder stb = new StringBuilder();
        if (getParameters() != null) {
            stb.append("p_");
        }
        stb.append(getDescription()).append('(')
                .append(Evaluator.commas(tuple, 0));
        if (getParameters() != null) {
            stb.append(", ").append(getParameters());
        }

        try {
            return Evaluator.evaluate(stb.append(')').toString());
        } catch (ScriptException e) {
            throw new IllegalStateException(e);
        }

    }
}
