package cspom.variable;

import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

public final class ExtensiveDomain implements Domain {
    private static final int DISPLAYED_VALUES = 3;
    private final List<Number> values;

    public ExtensiveDomain(Number... values) {
        this(Arrays.asList(values));
    }

    public ExtensiveDomain(List<Number> values) {
        this.values = values;
    }

    public List<Number> getValues() {
        return values;
    }

    @Override
    public String toString() {
        final Iterator<Number> itr = values.iterator();
        if (!itr.hasNext()) {
            return "[]";
        }
        final StringBuilder stb = new StringBuilder();
        stb.append('[');
        int max = DISPLAYED_VALUES;
        for (;;) {
            stb.append(itr.next());
            if (!itr.hasNext()) {
                return stb.append(']').toString();
            }
            if (--max == 0) {
                return stb.append("... (").append(
                        values.size() - DISPLAYED_VALUES).append(" more)]")
                        .toString();
            }
            stb.append(", ");
        }
    }
}
