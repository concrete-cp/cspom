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

package cspom.variable;

import java.util.ArrayList;
import java.util.List;

public final class ExtensiveDomain implements Domain {

	private final List<Number> values;

	private final String name;
	
	private final int nbValues;

	public ExtensiveDomain(final String name, final List<Number> values) {
		super();
		this.values = new ArrayList<Number>(values);
		this.name = name;
		this.nbValues = values.size();
	}

	public List<Number> getValues() {
		return values;
	}

	public String getName() {
		return name;
	}

	public int getNbValues() {
		return nbValues;
	}
	
	@Override
	public String toString() {
		final StringBuilder stb = new StringBuilder(name);
		stb.append(": [").append(values.size()).append(']');
		// for (Number value : values) {
		// stb.append(value).append(", ");
		// }
		// stb.delete(stb.length() - 2, stb.length()).append(']');
		return stb.toString();
	}

}
