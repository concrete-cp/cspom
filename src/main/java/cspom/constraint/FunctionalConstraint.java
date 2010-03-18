package cspom.constraint;

import cspom.variable.CSPOMVariable;

public final class FunctionalConstraint extends AbstractConstraint {

    private CSPOMVariable result;

    private CSPOMVariable[] arguments;

    public FunctionalConstraint(CSPOMVariable result, String function,
            CSPOMVariable... arguments) {
        super(function, concatenate(result, arguments));
        this.result = result;
        this.arguments = arguments;

    }

    private static CSPOMVariable[] concatenate(CSPOMVariable element,
            CSPOMVariable[] array) {
        CSPOMVariable[] result = new CSPOMVariable[array.length + 1];
        result[0] = element;
        System.arraycopy(array, 0, result, 1, array.length);
        return result;
    }

    public CSPOMVariable getResultVariable() {
        return result;
    }

    public CSPOMVariable[] getArguments() {
        return arguments;
    }

    @Override
    public String toString() {
        final StringBuilder stb = new StringBuilder();
        stb.append(result).append(" = ").append(getDescription());

        int iMax = arguments.length - 1;
        if (iMax == -1) {
            return stb.append("()").toString();
        }

        stb.append('(');
        for (int i = 0;; i++) {
            stb.append(arguments[i]);
            if (i == iMax) {
                return stb.append(')').toString();
            }
            stb.append(", ");
        }
    }

    @Override
    public void replaceVar(CSPOMVariable merged, CSPOMVariable var) {
        super.replaceVar(merged, var);
        if (result == merged) {
            result = var;
        }
        for (int i = arguments.length; --i >= 0;) {
            if (arguments[i] == merged) {
                arguments[i] = var;
            }
        }
    }

}
