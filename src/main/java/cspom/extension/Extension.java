/**
 * CSPFJ Competitor - CSP solver using the CSPFJ API for Java
 * Copyright (C) 2006 Julien VION
 * 
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, 
 * MA  02110-1301  USA
 */

package cspom.extension;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;

/**
 * This class is used to represent extensions, that is list of tuples allowed or
 * forbidden by a constraint.
 * 
 * @param <T>
 *            Type of recorded tuples.
 * 
 * @author vion
 * 
 */
public final class Extension<T> {

    /**
     * Arity of the extension.
     */
    private final int arity;

    /**
     * Whether tuples are initially set to true (allowed) or false (forbidden).
     */
    private final boolean init;

    /**
     * The actual allowed or forbidden tuples.
     */
    private final Collection<T[]> tuples;

    public Extension(final int arity, final boolean init) {
        super();
        this.arity = arity;
        this.init = init;
        this.tuples = new ArrayList<T[]>();
    }

    public void addTuple(final T[] tuple) {
        assert tuple.length == arity;
        tuples.add(tuple.clone());
    }

    public int getArity() {
        return arity;
    }

    public boolean init() {
        return init;
    }

    public Collection<T[]> getTuples() {
        return tuples;
    }

    public String tupleString() {
        final StringBuilder stb = new StringBuilder();
        for (T[] tuple : tuples) {
            for (T value : tuple) {
                stb.append(value).append(' ');
            }
            stb.delete(stb.length() - 1, stb.length()).append('|');
        }
        return stb.delete(stb.length() - 1, stb.length()).toString();
    }

    @Override
    public String toString() {
        return arity + "-ary, " + tuples.size() + " tuples, "
                + (init ? "conflicts" : "supports");
    }

    public int getNbTuples() {
        return tuples.size();
    }

    public boolean evaluate(final T[] values) {
        for (T[] tuple : tuples) {
            if (Arrays.equals(tuple, values)) {
                return !init;
            }
        }
        return init;
    }

    public Extension<T> reverse(final int[] newOrder) {
        final Extension<T> reversed = new Extension<T>(arity, init);

        for (T[] tuple : tuples) {
            final T[] reversedTuple = tuple.clone();
            for (int j = arity; --j >= 0;) {
                reversedTuple[j] = tuple[newOrder[j]];
            }
            reversed.addTuple(reversedTuple);
        }

        assert reversed.getNbTuples() == getNbTuples();
        return reversed;
    }
}
