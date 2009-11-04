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

package cspom.xcsp;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.xml.sax.Attributes;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.helpers.DefaultHandler;

import cspom.CSPOM;
import cspom.DuplicateVariableException;
import cspom.extension.Extension;
import cspom.extension.ExtensionConstraint;
import cspom.variable.CSPOMVariable;
import cspom.variable.Domain;
import cspom.variable.ExtensiveDomain;
import cspom.variable.Interval;

public final class ProblemHandler extends DefaultHandler {

	/**
	 * List of domains.
	 */
	private final Map<String, Object> domains;

	/**
	 * List of relations defining the constraints in extension.
	 */
	private final Map<String, Extension> relations;

	private final Map<String, Predicate> predicates;

	private Position position = Position.UNKNOWN;

	private StringBuilder contents;

	private final Map<String, String> currentAttributes;

	private Locator locator;

	private StringBuilder predicateContents;

	private final CSPOM problem;

	public ProblemHandler(final CSPOM problem) {
		super();

		this.problem = problem;
		currentAttributes = new HashMap<String, String>();
		domains = new HashMap<String, Object>();
		relations = new HashMap<String, Extension>();
		predicates = new HashMap<String, Predicate>();

		contents = new StringBuilder();

	}

	@Override
	public void setDocumentLocator(final Locator locator) {
		this.locator = locator;
	}

	private void addVariable(final String name, final String domain)
			throws ParseException {
		final Object dom = domains.get(domain);
		final CSPOMVariable variable;
		if (dom instanceof Interval) {
			variable = new CSPOMVariable(name, (Interval) dom);
		} else if (dom instanceof List<?>) {
			variable = new CSPOMVariable(name, (List<Number>) dom);
		} else {
			throw new IllegalArgumentException(domain + " not recognized");
		}

		try {
			problem.addVariable(variable);
		} catch (DuplicateVariableException e) {
			throw new ParseException("Variable " + variable
					+ " is defined twice", locator.getLineNumber());
		}
	}

	private void addConstraint(final String name, final String varNames,
			final String parameters, final String reference)
			throws ParseException {
		final String[] scopeList = varNames.split(" +");
		final CSPOMVariable[] scope = new CSPOMVariable[scopeList.length];
		for (int i = 0; i < scopeList.length; i++) {
			scope[i] = problem.getVariable(scopeList[i]);
			if (scope[i] == null) {
				throw new ParseException("Could not find variable "
						+ scopeList[i] + " from the scope of " + name, locator
						.getLineNumber());
			}
		}

		if (reference.startsWith("global:")) {
			final StringBuilder stb = new StringBuilder(reference.substring(7));
			stb.append("(").append(scope[0]);
			for (int i = 1; i < scope.length; i++) {
				stb.append(", ").append(scope[i]);
			}
			stb.append(")");
			problem.ctr(stb.toString());
			return;
		}

		final Extension extension = relations.get(reference);
		if (extension != null) {
			problem.addConstraint(new ExtensionConstraint(name, extension,
					scope));
			return;
		}

		final Predicate predicate = predicates.get(reference);
		if (predicate == null) {
			throw new ParseException("Unknown reference " + reference, locator
					.getLineNumber());
		}

		problem.ctr(predicate.applyParameters(parameters, scope));
	}

	/**
	 * Generate variable and constraints from the XML data.
	 * 
	 * @see org.xml.sax.ContentHandler#startElement(java.lang.String,
	 *      java.lang.String, java.lang.String, org.xml.sax.Attributes)
	 */
	@Override
	public void startElement(final String uri, final String localName,
			final String qName, final Attributes attributes)
			throws SAXParseException {
		if ("domain".equals(qName)) {
			position = Position.DOMAIN;
			copyAttributes(attributes, new String[] { "name" });

		} else if ("variable".equals(qName)) {

			try {
				addVariable(attributes.getValue("name"), attributes
						.getValue("domain"));
			} catch (ParseException e) {
				throw new SAXParseException("Could not create variable",
						locator, e);
			}

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
	public void characters(final char[] characters, final int start,
			final int length) {

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

				parseDomain(name, contents.toString());
				// logger.finer(domain.toString());
			}
			break;

		case RELATION:
			assert "relation".equals(qName);
			{
				final String name = currentAttributes.get("name");

				Extension relation;
				try {
					relation = new Extension(name, Integer
							.parseInt(currentAttributes.get("arity")), Integer
							.parseInt(currentAttributes.get("nbTuples")),
							currentAttributes.get("semantics"), contents
									.toString());
				} catch (NumberFormatException e) {
					throw new SAXParseException("Could not read number",
							locator, e);
				} catch (ParseException e) {
					throw new SAXParseException("Error parsing tuples",
							locator, e);
				}

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
				final Predicate predicate = new Predicate(predicateContents
						.toString(), contents.toString());

				predicates.put(name, predicate);
				// logger.finer(predicate.toString());
			}
			position = Position.PREDICATE;
			return;

		case CONSTRAINT:
			if (!"parameters".equals(qName) && !"constraint".equals(qName)) {
				throw new SAXParseException("Unknown tag " + qName, locator);
			}

			{
				final String name = currentAttributes.get("name");

				try {
					addConstraint(name, currentAttributes.get("scope"),
							contents.toString(), currentAttributes
									.get("reference"));
				} catch (ParseException e) {
					throw new SAXParseException("Could not create constraint "
							+ name, locator, e);
				}

				// logger.finer(constraint.toString());

			}
			break;

		default:
		}

		position = Position.UNKNOWN;

	}

	private void parseDomain(final String name, final String domain)
			throws SAXParseException {

		final String[] listOfValues = domain.trim().split(" +");

		if (listOfValues.length == 1 && listOfValues[0].contains("..")) {
			final String[] fromto = listOfValues[0].trim().split("\\.\\.");
			try {

				final int start = Integer.parseInt(fromto[0]);
				final int end = Integer.parseInt(fromto[1]);
				domains.put(name, new Interval(start, end));

			} catch (NumberFormatException e) {
				throw new SAXParseException(e.toString(), locator);
			}

		} else {

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

			domains.put(name, values);

		}

	}

	private enum Position {
		DOMAIN, RELATION, PREDICATE, PREDICATE_PARAMETERS, PREDICATE_EXPRESSION, CONSTRAINT, UNKNOWN
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

}
