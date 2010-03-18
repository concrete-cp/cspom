package cspom.constraint;

import cspom.variable.CSPOMVariable;

public final class GeneralConstraint extends AbstractConstraint {

    public GeneralConstraint(String name, String description,
            CSPOMVariable... scope) {
        super(name, description, scope);
    }

    public GeneralConstraint(String description, CSPOMVariable... scope) {
        super(description, scope);
    }

    public String toString() {
        final StringBuilder stb = new StringBuilder();
        stb.append(getDescription());

        int iMax = getScope().size() - 1;
        if (iMax == -1) {
            return stb.append("()").toString();
        }

        stb.append('(');
        for (int i = 0;; i++) {
            stb.append(getVariable(i));
            if (i == iMax) {
                return stb.append(')').toString();
            }
            stb.append(", ");
        }
    }
    

}
