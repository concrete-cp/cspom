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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 */

package cspom.extension;

import java.util.Arrays;

public final class Extension {

	// private static final Logger logger = Logger
	// .getLogger("competitor.problem.relation");

	private final int arity;

	private final boolean supports;

	private final Number[][] tuples;

	private final int nbTuples;

	public Extension(final int arity, final int nbTuples,
			final boolean supports, final Number[][] tuples) {
		super();
		this.arity = arity;
		this.supports = supports;
		this.tuples = tuples;
		this.nbTuples = nbTuples;
	}

	public Extension(final int arity, final int nbTuples,
			final String semantics, final Number[][] tuples) {
		this(arity, nbTuples, "supports".equals(semantics), tuples);
	}

	public int getArity() {
		return arity;
	}

	public boolean isSupports() {
		return supports;
	}

	public Number[][] getTuples() {
		return tuples;
	}

	public String tupleString() {
		final StringBuilder stb = new StringBuilder();
		for (Number[] tuple : tuples) {
			for (Number value : tuple) {
				stb.append(value).append(' ');
			}
			stb.delete(stb.length() - 1, stb.length()).append('|');
		}
		return stb.delete(stb.length() - 1, stb.length()).toString();
	}

	public String toString() {

		return super.toString() + ": " + arity + "-ary, " + tuples.length
				+ " tuples, " + (supports ? "supports" : "conflicts");// + ": "
																		// +
		// tupleString();

	}

	public int getNbTuples() {
		return nbTuples;
	}

	public boolean evaluate(final Number[] values) {
		for (Number[] tuple : tuples) {
			if (Arrays.equals(tuple, values)) {
				return supports;
			}
		}
		return !supports;
	}

	public Extension reverse(final int[] newOrder) {
		final Number[][] tuples = new Number[this.tuples.length][this.tuples[0].length];
		for (int i = tuples.length; --i >= 0;) {
			for (int j = tuples[i].length; --j >= 0;) {
				tuples[i][j] = this.tuples[i][newOrder[j]];
			}
		}
		return new Extension(arity, nbTuples, supports, tuples);
	}
}
