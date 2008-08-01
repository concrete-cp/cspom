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

package cspom;

import java.security.InvalidParameterException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import javax.script.ScriptException;

import org.xml.sax.Attributes;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.helpers.DefaultHandler;

import cspom.constraint.Constraint;
import cspom.constraint.GlobalConstraint;
import cspom.extension.Extension;
import cspom.extension.ExtensionConstraint;
import cspom.predicate.Predicate;
import cspom.predicate.PredicateConstraint;
import cspom.variable.Domain;
import cspom.variable.ExtensiveDomain;
import cspom.variable.IntervalDomain;
import cspom.variable.Variable;

public final class ProblemHandler extends DefaultHandler {

	/**
	 * Liste des domaines.
	 */
	private final Map<String, Domain> domains;

	/**
	 * Liste des variables.
	 */
	private final Map<String, Variable> variables;

	/**
	 * Liste des relations définissant les contraintes.
	 */
	private final Map<String, Extension> relations;

	private final Map<String, Predicate> predicates;

	/**
	 * Liste des contraintes.
	 */
	private final Map<String, Constraint> constraints;

	private Position position = Position.UNKNOWN;

	private StringBuilder contents;

	private final Map<String, String> currentAttributes;

	private final static Logger logger = Logger
			.getLogger("csploader.ProblemHandler");

	private Locator locator;

	private StringBuilder predicateContents;

	private final Problem problem;

	public ProblemHandler(final Problem problem) {
		super();

		this.problem = problem;
		currentAttributes = new HashMap<String, String>();
		domains = new HashMap<String, Domain>();
		relations = new HashMap<String, Extension>();
		predicates = new HashMap<String, Predicate>();
		variables = new HashMap<String, Variable>();
		// orderedVariables = new ArrayList<Variable>();
		constraints = new HashMap<String, Constraint>();

		contents = new StringBuilder();

	}

	@Override
	public void setDocumentLocator(final Locator locator) {
		this.locator = locator;
	}

	private void addVariable(final String name, final String domain) {
		final Variable variable = new Variable(name, domains.get(domain));
		variables.put(name, variable);
		problem.addVariable(variable);
		// logger.finer(variable.toString());
	}

	private Constraint createConstraint(final String name,
			final String varNames, final String parameters,
			final String reference) throws SAXParseException {
		final String[] scopeList = varNames.split(" +");
		final List<Variable> scope = new ArrayList<Variable>(scopeList.length);
		for (int i = 0; i < scopeList.length; i++) {
			scope.add(variables.get(scopeList[i]));
		}

		if (reference.startsWith("global:")) {
			return new GlobalConstraint(name, scope, reference.substring(7));
		}

		final Extension extension = relations.get(reference);
		if (extension != null) {
			return new ExtensionConstraint(name, scope, extension);

		}

		final Predicate predicate = predicates.get(reference);
		if (predicate == null) {
			throw new SAXParseException("Unknown reference " + reference,
					locator);
		}

		return new PredicateConstraint(name, scope, parameters, predicates
				.get(reference));
	}

	/**
	 * Génère variables et contraintes à partir du fichier XML.
	 * 
	 * @see org.xml.sax.ContentHandler#startElement(java.lang.String,
	 *      java.lang.String, java.lang.String, org.xml.sax.Attributes)
	 */
	@Override
	public void startElement(final String uri, final String localName,
			final String qName, final Attributes attributes) {

		if ("domain".equals(qName)) {
			position = Position.DOMAIN;
			copyAttributes(attributes, new String[] { "name" });

		} else if ("variable".equals(qName)) {

			addVariable(attributes.getValue("name"), attributes
					.getValue("domain"));

		} else if ("relation".equals(qName)) {
			position = Position.RELATION;

			copyAttributes(attributes, new String[] { "name", "nbTuples",
					"arity", "semantics" });

		} else if ("predicate".equals(qName)) {

			position = Position.PREDICATE;

			copyAttributes(attributes, new String[] { "name" });

			predicateContents = new StringBuilder();

		} else if ("parameters".equals(qName)
				&& Position.PREDICATE.equals(position)) {

			position = Position.PREDICATE_PARAMETERS;

		} else if ("expression".equals(qName)
				&& Position.PREDICATE.equals(position)) {

			position = Position.PREDICATE_EXPRESSION;

		} else if ("constraint".equals(qName)) {

			position = Position.CONSTRAINT;

			copyAttributes(attributes, new String[] { "name", "arity", "scope",
					"reference" });

		}

	}

	private void copyAttributes(final Attributes attributes, final String[] keys) {
		contents = new StringBuilder();
		currentAttributes.clear();

		for (String key : keys) {
			currentAttributes.put(key, attributes.getValue(key));
		}
	}

	@Override
	public void characters(final char[] characters, final int start, final int length) {

		if (position == Position.PREDICATE_PARAMETERS) {
			predicateContents.append(characters, start, length);
		} else {
			contents.append(characters, start, length);
		}
	}

	@Override
	public void endElement(final String uri, final String localName,
			final String qName) throws SAXException {
		super.endElement(uri, localName, qName);

		switch (position) {
		case DOMAIN:
			assert "domain".equals(qName);
			{
				final String name = currentAttributes.get("name");

				final Domain domain = parseDomain(name, contents.toString());

				domains.put(name, domain);
				// logger.finer(domain.toString());
			}
			break;

		case RELATION:
			assert "relation".equals(qName);
			{
				final String name = currentAttributes.get("name");

				final Extension relation = parseRelation(name, Integer
						.parseInt(currentAttributes.get("arity")), Integer
						.parseInt(currentAttributes.get("nbTuples")),
						currentAttributes.get("semantics"), contents.toString());

				relations.put(name, relation);
				// logger.finer(relation.toString());
			}
			break;

		case PREDICATE_PARAMETERS:
			assert "parameters".equals(qName);
			position = Position.PREDICATE;
			return;

		case PREDICATE_EXPRESSION:
			assert "functional".equals(qName);
			{
				final String name = currentAttributes.get("name");
				final Predicate predicate;
				try {
					predicate = parsePredicate(name, predicateContents
							.toString(), contents.toString());
				} catch (ScriptException e) {
					logger.throwing(ProblemHandler.class.getSimpleName(),
							"end", e);
					throw new SAXParseException(e.toString(), locator);
				}

				predicates.put(name, predicate);
				// logger.finer(predicate.toString());
			}
			position = Position.PREDICATE;
			return;

		case CONSTRAINT:
			assert "parameters".equals(qName) || "constraint".equals(qName);
			{
				final String name = currentAttributes.get("name");

				final Constraint constraint = createConstraint(name,
						currentAttributes.get("scope"), contents.toString(),
						currentAttributes.get("reference"));

				problem.addConstraint(constraint);
				constraints.put(name, constraint);

				// logger.finer(constraint.toString());

			}
			break;

			
		default:
		}

		position = Position.UNKNOWN;

	}

	private Domain parseDomain(final String name, final String domain)
			throws SAXParseException {

		final String[] listOfValues = domain.trim().split(" +");

		if (listOfValues.length == 1 && listOfValues[0].contains("..")) {
			final String[] fromto = listOfValues[0].trim().split("\\.\\.");
			try {

				final int start = Integer.parseInt(fromto[0]);
				final int end = Integer.parseInt(fromto[1]);
				return new IntervalDomain(name, start, end);

			} catch (NumberFormatException e) {
				throw new SAXParseException(e.toString(), locator);
			}

		}

		final List<Number> values = new ArrayList<Number>();

		for (String currentValue : listOfValues) {
			if (currentValue.contains("..")) {

				final String[] fromto = currentValue.split("\\.\\.");
				final int start = Integer.parseInt(fromto[0]);
				final int end = Integer.parseInt(fromto[1]);
				for (int i = 0; i <= end - start; i++) {
					values.add(i + start);
				}

			} else {

				values.add(Integer.parseInt(currentValue.trim()));

			}
		}
		return new ExtensiveDomain(name, values);
	}

	private Number[][] parseTuples(final int arity, final int nbTuples,
			final String string) throws SAXParseException {

		if (nbTuples < 1) {
			return new Number[0][];
		}

		final Number[][] tuples = new Number[nbTuples][arity];

		final String[] tupleList = string.split("\\|");

		if (tupleList.length != nbTuples) {
			throw new SAXParseException("Inconsistent number of Tuples ("
					+ tupleList.length + " /= " + nbTuples + ") in " + string,
					locator);
		}

		for (int i = nbTuples; --i >= 0;) {

			final String[] valueList = tupleList[i].trim().split(" +");

			if (valueList.length != arity) {
				throw new SAXParseException("Incorrect arity ("
						+ valueList.length + " /= " + arity + ") in "
						+ tupleList[i].trim(), locator);
			}

			for (int j = arity; --j >= 0;) {
				tuples[i][j] = Integer.parseInt(valueList[j]);
			}

		}

		logger.finest(tuplesToString(tuples));

		return tuples;
	}

	private String tuplesToString(final Number[][] tuples) {
		final StringBuilder stb = new StringBuilder();
		stb.append("Tuples : [");
		if (tuples.length > 0) {
			stb.append(Arrays.toString(tuples[0]));
		}
		for (int i = 1; i < tuples.length; i++) {
			stb.append(", ").append(Arrays.toString(tuples[i]));
		}

		stb.append(']');
		return stb.toString();
	}

	private Extension parseRelation(final String name, final int arity,
			final int nbTuples, final String semantics, final String relation)
			throws SAXParseException {
		final Number[][] tuples = parseTuples(arity, nbTuples, relation);

		return new Extension(name, arity, nbTuples, semantics, tuples);
	}

	private Predicate parsePredicate(final String name,
			final String parameters, final String expression)
			throws ScriptException {
		return new Predicate(name, parameters, expression);
	}

	private enum Position {
		DOMAIN, RELATION, PREDICATE, PREDICATE_PARAMETERS, PREDICATE_EXPRESSION, CONSTRAINT, UNKNOWN
	}

	public int getNbConstraints() {
		return constraints.size();
	}

	public int getNbDomains() {
		return domains.size();
	}

	public int getNbPredicates() {
		return predicates.size();
	}

	public int getNbRelations() {
		return relations.size();
	}

	public int getNbVariables() {
		return variables.size();
	}

}
