/**
 *  CSP Object Model (CSPOM) - a CSP modeling API
 *  Copyright (C) 2008-2010 Julien Vion
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.

 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, 
 *  MA 02110-1301  USA
 */
package cspom;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.GZIPInputStream;

import org.apache.tools.bzip2.CBZip2InputStream;

import cspom.compiler.ConstraintCompiler;
import cspom.compiler.PredicateParseException;
import cspom.constraint.CSPOMConstraint;
import cspom.constraint.FunctionalConstraint;
import cspom.variable.BooleanDomain;
import cspom.variable.CSPOMDomain;
import cspom.variable.CSPOMVariable;
import cspom.xcsp.Parser;
import cspom.xcsp.XCSPParseException;

/**
 * 
 * CSPOM is the central class of the Constraint Satisfaction Problem Object
 * Model. You can create a problem from scratch by instantiating this class and
 * then using the ctr and var methods. The CSPOM.load() method can be used in
 * order to create a CSPOM object from an XCSP file.
 * <p>
 * The CSPOM class adheres to the following definition :
 * 
 * A CSP is defined as a pair (X, C), X being a finite set of variables, and C a
 * finite set of constraints.
 * 
 * A domain is associated to each variable x in X. Each constraint involves a
 * finite set of variables (its scope) and defines a set of allowed and
 * forbidden instantiations of these variables.
 * 
 * @author Julien Vion
 * @see CSPOMConstraint
 * @see CSPOMVariable
 * 
 */
public final class CSPOM {

	/**
	 * Version obtained from SVN Info.
	 */
	public static final String VERSION;

	static {
		final Matcher matcher = Pattern.compile("Rev:\\ (\\d+)").matcher(
				"$Rev$");
		matcher.find();
		VERSION = matcher.group(1);
	}

	/**
	 * List of variables of the problem, in order.
	 */
	private final List<CSPOMVariable> variableList;

	/**
	 * Map used to easily retrieve a variable according to its name.
	 */
	private final Map<String, CSPOMVariable> variableMap;

	/**
	 * Collection of all constraints of the problem.
	 */
	private final Collection<CSPOMConstraint> constraints;

	/**
	 * The constraint compiler used by this CSPOM instance.
	 */
	private ConstraintCompiler constraintCompiler;

	/**
	 * Creates an empty problem, without any initial variables nor constraints.
	 */
	public CSPOM() {
		variableList = new LinkedList<CSPOMVariable>();
		variableMap = new HashMap<String, CSPOMVariable>();
		constraints = new ArrayList<CSPOMConstraint>();
	}

	/**
	 * Opens an InputStream according to the given URL. If URL ends with ".gz"
	 * or ".bz2", the stream is inflated accordingly.
	 * 
	 * @param url
	 *            URL to open
	 * @return An InputStream corresponding to the given URL
	 * @throws IOException
	 *             If the InputStream could not be opened
	 */
	public static InputStream problemInputStream(final URL url)
			throws IOException {

		final String path = url.getPath();

		if (path.endsWith(".gz")) {
			return new GZIPInputStream(url.openStream());
		}

		if (path.endsWith(".bz2")) {
			final InputStream is = url.openStream();
			is.read();
			is.read();
			return new CBZip2InputStream(is);
		}

		return url.openStream();
	}

	/**
	 * Loads a CSPOM from a given XCSP file.
	 * 
	 * @param xcspFile
	 *            Either a filename or an URI. Filenames ending with .gz or .bz2
	 *            will be inflated accordingly.
	 * @return The loaded CSPOM
	 * @throws XCSPParseException
	 *             If the given file could not be parsed.
	 * @throws IOException
	 *             If the given file could not be read.
	 */
	public static CSPOM load(final String xcspFile) throws XCSPParseException,
			IOException {
		final URI uri;
		try {
			uri = new URI(xcspFile);
		} catch (URISyntaxException e) {
			throw new IOException("Invalid URI", e);
		}

		if (uri.getScheme() == null) {
			return load(new URL("file://" + uri));
		}

		return load(uri.toURL());
	}

	/**
	 * Loads a CSPOM from a given XCSP file.
	 * 
	 * @param url
	 *            An URL locating the XCSP file. Filenames ending with .gz or
	 *            .bz2 will be inflated accordingly.
	 * @return The loaded CSPOM
	 * @throws XCSPParseException
	 *             If the given file could not be parsed.
	 * @throws IOException
	 *             If the given file could not be read.
	 */
	public static CSPOM load(final URL url) throws XCSPParseException,
			IOException {

		final CSPOM problem = new CSPOM();
		final InputStream problemIS = problemInputStream(url);

		new Parser(problem).parse(problemIS);
		// problem.compile();
		return problem;
	}

	public void compile() {
		final Collection<CSPOMConstraint> toRemove = new ArrayList<CSPOMConstraint>();

		for (CSPOMVariable v : getVariables()) {
			if (BooleanDomain.TRUE.equals(v.getDomain())) {
				for (CSPOMConstraint c : v.getConstraints()) {
					if (!(c instanceof FunctionalConstraint)
							|| !"and".equals(c.getDescription())) {
						continue;
					}
					final FunctionalConstraint fc = (FunctionalConstraint) c;
					if (fc.getResultVariable() == v) {
						for (CSPOMVariable a : fc.getArguments()) {
							a.setDomain(BooleanDomain.TRUE);
						}
						toRemove.add(c);
					}
				}
			}
		}

		for (CSPOMConstraint c : toRemove) {
			removeConstraint(c);
		}

	}

	private void removeConstraint(final CSPOMConstraint c) {
		for (CSPOMVariable v : c.getScope()) {
			v.removeConstraint(c);
		}
		constraints.remove(c);

	}

	public void removeVariable(final CSPOMVariable v) {
		if (v.getConstraints().isEmpty()) {
			variableList.remove(v);
			variableMap.remove(v.getName());
		} else {
			throw new IllegalArgumentException(v
					+ " is still implied by constraints");
		}

	}

	/**
	 * @return The variables of this problem.
	 */
	public List<CSPOMVariable> getVariables() {
		return variableList;
	}

	/**
	 * Adds a variable to the problem.
	 * 
	 * @param variable
	 *            The variable to add. It must have an unique name.
	 * @throws DuplicateVariableException
	 *             If a variable with the same name already exists.
	 */
	public void addVariable(final CSPOMVariable variable)
			throws DuplicateVariableException {
		if (variableMap.put(variable.getName(), variable) != null) {
			throw new DuplicateVariableException(variable.getName());
		}
		variableList.add(variable);

	}

	/**
	 * Adds a constraint to the problem.
	 * 
	 * @param constraint
	 *            The constraint to add.
	 */
	public void addConstraint(final CSPOMConstraint constraint) {
		constraints.add(constraint);
	}

	/**
	 * @return The constraints of this problem.
	 */
	public Collection<CSPOMConstraint> getConstraints() {
		return constraints;
	}

	/**
	 * Adds a bounded, unnamed variable in the problem.
	 * 
	 * @param lb
	 *            lower bound
	 * @param ub
	 *            upper bound
	 * @return The added variable.
	 */
	public CSPOMVariable var(final int lb, final int ub) {
		final CSPOMVariable variable = new CSPOMVariable(lb, ub);
		try {
			addVariable(variable);
		} catch (DuplicateVariableException e) {
			throw new IllegalStateException();
		}
		return variable;
	}

	/**
	 * Adds a bounded, named variable in the problem.
	 * 
	 * @param name
	 *            name of the variable
	 * @param lb
	 *            lower bound
	 * @param ub
	 *            upper bound
	 * @return The added variable.
	 * @throws DuplicateVariableException
	 *             if a variable of the same name already exists
	 */
	public CSPOMVariable var(final String name, final int lb, final int ub)
			throws DuplicateVariableException {
		final CSPOMVariable variable = new CSPOMVariable(name, lb, ub);
		addVariable(variable);
		return variable;
	}

	/**
	 * Adds functional constraints to the problem. The given predicate will be
	 * parsed and the appropriate constraints added to the problem. The
	 * predicate may be complex and is usually translated to a set of
	 * constraints and auxiliary variables, resulting in the returned
	 * subproblem. Use variable names in the predicate to reference them. These
	 * variables must already be added to the problem.
	 * 
	 * @param string
	 *            A predicate.
	 * @throws PredicateParseException
	 *             If an error was encountered parsing the predicate.
	 * @return The subproblem corresponding to the given predicate
	 */
	public CSPOM ctr(final String string) throws PredicateParseException {
		if (constraintCompiler == null) {
			constraintCompiler = new ConstraintCompiler(this);
		}
		final CSPOM subproblem = constraintCompiler.split(string);
		merge(subproblem);
		return subproblem;
	}

	/**
	 * @param variableName
	 *            A variable name.
	 * @return The variable with the corresponding name.
	 */
	public CSPOMVariable getVariable(final String variableName) {
		return variableMap.get(variableName);
	}

	/**
	 * Merges the given subproblem in the current problem. If not already
	 * present, variables are added to the current problem.
	 * 
	 * @param problem
	 *            A subproblem.
	 */
	public void merge(final CSPOM problem) {
		for (CSPOMVariable v : problem.variableList) {
			if (!variableMap.containsValue(v)) {
				try {
					addVariable(v);
				} catch (DuplicateVariableException e) {
					throw new IllegalStateException();
				}
			}
		}

		constraints.addAll(problem.constraints);
	}

	@Override
	public String toString() {
		final StringBuilder stb = new StringBuilder();
		for (CSPOMVariable v : variableList) {
			stb.append(v).append(" = ");
			final CSPOMDomain<?> d = v.getDomain();
			if (d == null) {
				stb.append('?');
			} else {
				stb.append(d);
			}
			stb.append('\n');
		}
		for (CSPOMConstraint c : constraints) {
			stb.append(c).append('\n');
		}
		return stb.toString();
	}

	/**
	 * Generates the constraint network graph in the GML format. N-ary
	 * constraints are represented as nodes.
	 * 
	 * @return a String containing the GML representation of the constraint
	 *         network.
	 */
	public String toGML() {
		final StringBuilder stb = new StringBuilder();
		stb.append("graph [\n");
		stb.append("directed 0\n");
		for (CSPOMVariable v : variableList) {
			stb.append("node [\n");
			stb.append("id \"").append(v.getName()).append("\"\n");
			stb.append("label \"").append(v.getName()).append("\"\n");
			stb.append("]\n");
		}

		int gen = 0;
		for (CSPOMConstraint c : constraints) {
			if (c.getArity() > 2) {
				stb.append("node [\n");
				stb.append("id \"cons").append(gen).append("\"\n");
				stb.append("label \"").append(c.getDescription())
						.append("\"\n");
				stb.append("graphics [\n");
				stb.append("fill \"#FFAA00\"\n");
				stb.append("]\n");
				stb.append("]\n");

				for (CSPOMVariable v : c.getScope()) {
					stb.append("edge [\n");
					stb.append("source \"cons").append(gen).append("\"\n");
					stb.append("target \"").append(v.getName()).append("\"\n");
					stb.append("]\n");
				}
				gen++;
			} else if (c.getArity() == 2) {
				stb.append("edge [\n");
				stb.append("source \"").append(c.getScope().get(0)).append(
						"\"\n");
				stb.append("target \"").append(c.getScope().get(1)).append(
						"\"\n");
				stb.append("label \"").append(c.getDescription())
						.append("\"\n");
				stb.append("]\n");
			}
		}
		stb.append("]\n");

		return stb.toString();
	}
}
