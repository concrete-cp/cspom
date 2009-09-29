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

import java.util.Arrays;
import java.util.List;

public final class SingletonDomain implements Domain {

	private final Number value;

	public SingletonDomain(final Number value) {
		super();
		this.value = value;
	}

	public List<Number> getValues() {
		return Arrays.asList(value);
	}

	public String getName() {
		return null;
	}

	public int getNbValues() {
		return 1;
	}

	@Override
	public String toString() {
		return "{" + value + "}";
	}

}
