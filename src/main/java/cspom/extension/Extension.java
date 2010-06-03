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

import java.util.Iterator;
import java.util.Set;

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
    private final Set<T[]> tuples;

    /**
     * Constructs an empty (universal or empty) extension.
     * 
     * @param arity
     *            arity of the extension.
     * @param init
     *            initial value of tuples: true (allowed) or false (forbidden).
     */
    public Extension(final int arity, final boolean init) {
        super();
        this.arity = arity;
        this.init = init;
        this.tuples = new TupleSet<T>();
    }

    /**
     * Adds an allowed or forbidden tuple to the extension.
     * 
     * @param tuple
     *            the tuple to allow or forbid.
     */
    public void addTuple(final T... tuple) {
        assert tuple.length == arity;
        tuples.add(tuple.clone());
    }
    
    

    /**
     * @return the arity of the extension.
     */
    public int getArity() {
        return arity;
    }

    /**
     * @return whether initial value of tuples is true or false.
     */
    public boolean init() {
        return init;
    }

    /**
     * @return all forbidden or allowed tuples.
     */
    public Set<T[]> getTuples() {
        return tuples;
    }

    /**
     * @return string representation of tuples in format "a b c...|d e f...|..."
     */
    public String tupleString() {
        final Iterator<T[]> itr = tuples.iterator();
        if (!itr.hasNext()) {
            return "";
        }
        final StringBuilder stb = new StringBuilder();
        for (;;) {
            stb.append(tupleToString(itr.next()));
            if (!itr.hasNext()) {
                return stb.toString();
            }
            stb.append('|');
        }
    }

    /**
     * Returns a String representation of a given tuple in the form "a b c...".
     * 
     * @param <T>
     *            Type of values in the tuple
     * @param tuple
     *            the tuple
     * @return the String representation
     */
    private static <T> String tupleToString(final T... tuple) {
        if (tuple.length == 0) {
            return "";
        }
        final StringBuilder stb = new StringBuilder();

        for (int i = 0;;) {
            stb.append(tuple[i++]);
            if (i >= tuple.length) {
                return stb.toString();
            }
            stb.append(' ');
        }
    }

    @Override
    public String toString() {
        final String stb = arity + "-ary, " + tuples.size() + " tuples, ";
        if (init) {
            return stb + "conflicts";
        }
        return stb + "supports";
    }

    /**
     * @return number of allowed or forbidden tuples.
     */
    public int getNbTuples() {
        return tuples.size();
    }

    /**
     * @param tuple
     *            a tuple
     * @return whether the given tuple is allowed or forbidden.
     */
    public boolean evaluate(final T... tuple) {
        return init ^ tuples.contains(tuple);
    }

    /**
     * This method returns a copy of this extension with permuted tuples. New
     * order of tuples is given as an argument.
     * 
     * <p>
     * For example, a ternary extension 1 2 3|1 3 4|2 4 5 will be reversed to 1
     * 3 2|1 4 3|2 5 4 by a call to reverse(0, 2, 1).
     * </p>
     * 
     * @param newOrder
     *            new order of the extension.
     * @return a reversed copy of the extension.
     */
    public Extension<T> reverse(final int... newOrder) {
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
