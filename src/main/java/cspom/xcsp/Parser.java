package cspom.xcsp;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

import cspom.CSPOM;
import cspom.DuplicateVariableException;
import cspom.compiler.PredicateParseException;
import cspom.extension.Extension;
import cspom.extension.ExtensionConstraint;
import cspom.variable.CSPOMVariable;
import cspom.variable.CSPOMDomain;
import cspom.variable.ExtensiveDomain;
import cspom.variable.Interval;

/**
 * This class implements an XCSP 2.0 parser.
 * 
 * @author vion
 */
public final class Parser {

	/**
	 * XPath expression to retrieve predicate parameters.
	 */
	private static final XPathExpression PARAMS_EXPR;

	/**
	 * XPath expression to retrieve predicate functional expression.
	 */
	private static final XPathExpression FUNC_EXPR;
	static {
		final XPath xpath = XPathFactory.newInstance().newXPath();
		try {
			PARAMS_EXPR = xpath.compile("parameters");
			FUNC_EXPR = xpath.compile("expression/functional");
		} catch (XPathExpressionException e) {
			throw new IllegalStateException();
		}
	}

	/**
	 * The problem to which every variable or constraint will be added.
	 */
	private final CSPOM problem;

	/**
	 * Constructor.
	 * 
	 * @param problem
	 *            The problem to which every parsed variable or constraint will
	 *            be added.
	 */
	public Parser(final CSPOM problem) {
		this.problem = problem;
	}

	/**
	 * Append the XCSP data provided by the InputStream to the given CSPOM
	 * problem.
	 * 
	 * @param is
	 *            The source of the XCSP data
	 * @throws XCSPParseException
	 *             Thrown if is contains invalid data
	 * @throws IOException
	 *             Thrown if the data could not be read
	 */
	public void parse(final InputStream is) throws XCSPParseException,
			IOException {
		DocumentBuilder db;
		try {
			db = DocumentBuilderFactory.newInstance().newDocumentBuilder();
		} catch (ParserConfigurationException e) {
			throw new IllegalStateException(e);
		}

		final Document document;
		try {
			document = db.parse(is);
		} catch (SAXParseException e) {
			throw new XCSPParseException(e, e.getLineNumber());
		} catch (SAXException e) {
			throw new IllegalStateException(e);
		}

		final Map<String, CSPOMDomain<?>> domains = parseDomains(document);
		parseVariables(domains, document);
		final Map<String, Extension<?>> relations = parseRelations(document);
		final Map<String, Predicate> predicates = parsePredicates(document);
		parseConstraints(relations, predicates, document);
	}

	/**
	 * Parse domains described in the document.
	 * 
	 * @param doc
	 *            The document
	 * @return A map of all domains described in the document
	 */
	private static Map<String, CSPOMDomain<?>> parseDomains(final Document doc) {
		final Map<String, CSPOMDomain<?>> domainMap = new HashMap<String, CSPOMDomain<?>>();
		final NodeList domains = doc.getElementsByTagName("domain");
		for (int i = domains.getLength(); --i >= 0;) {
			final Node domainNode = domains.item(i);
			domainMap
					.put(domainNode.getAttributes().getNamedItem("name")
							.getTextContent(), parseDomain(domainNode
							.getTextContent()));

		}

		return domainMap;
	}

	/**
	 * Parse the given domain given as a String. Domains are usually sequence of
	 * values separated by spaces such as "1 3 -4 5" or intervals in the format
	 * "a..b". Sequences of values and intervals such as "1 3..10 18..30" are
	 * allowed and converted to a sequence of values.
	 * 
	 * @param domain
	 *            The String domain to parse
	 * @return The resulting Domain object
	 */
	private static CSPOMDomain<?> parseDomain(final String domain) {
		final String[] listOfValues = domain.trim().split(" +");

		if (listOfValues.length == 1 && listOfValues[0].contains("..")) {
			return interval(listOfValues[0]);
		}

		return extensiveDomain(listOfValues);
	}

	/**
	 * Parses a single interval given in form "a..b".
	 * 
	 * @param interval
	 *            interval to be parsed
	 * @return an Interval object corresponding to the given interval
	 */
	private static Interval<Integer> interval(final String interval) {
		final String[] fromto = interval.trim().split("\\.\\.");
		return new Interval<Integer>(Integer.parseInt(fromto[0]), Integer
				.parseInt(fromto[1]));
	}

	private static ExtensiveDomain<?> extensiveDomain(
			final String[] listOfValues) {

		final List<Integer> values = new ArrayList<Integer>();

		for (String currentValue : listOfValues) {
			if (currentValue.contains("..")) {
				values.addAll(interval(currentValue).getValues());
			} else {
				values.add(Integer.parseInt(currentValue.trim()));
			}
		}

		return new ExtensiveDomain<Integer>(values);
	}

	/**
	 * Parse variables and add them to the problem.
	 * 
	 * @param domains
	 *            Domains to use for variables.
	 * @param doc
	 *            XCSP document
	 * @throws XCSPParseException
	 *             If two variables have the same name.
	 */
	private void parseVariables(final Map<String, CSPOMDomain<?>> domains,
			final Document doc) throws XCSPParseException {
		final NodeList variables = doc.getElementsByTagName("variable");
		for (int i = 0; i < variables.getLength(); i++) {
			final NamedNodeMap variableAttributes = variables.item(i)
					.getAttributes();
			final String name = variableAttributes.getNamedItem("name")
					.getTextContent();
			try {
				problem.addVariable(new CSPOMVariable(name, domains
						.get(variableAttributes.getNamedItem("domain")
								.getTextContent())));
			} catch (DuplicateVariableException e) {
				throw new XCSPParseException("Variable " + name
						+ " is defined twice", e);
			}

		}
	}

	/**
	 * Parse extension relations and return them as a map. Keys are the
	 * relations' names.
	 * 
	 * @param doc
	 *            XCSP document
	 * @return The relations.
	 * @throws XCSPParseException
	 *             If there was an error parsing a relation.
	 */
	private static Map<String, Extension<?>> parseRelations(final Document doc)
			throws XCSPParseException {
		final Map<String, Extension<?>> relationMap = new HashMap<String, Extension<?>>();
		final NodeList relations = doc.getElementsByTagName("relation");
		for (int i = relations.getLength(); --i >= 0;) {
			final Node relationNode = relations.item(i);
			final NamedNodeMap attributes = relationNode.getAttributes();

			relationMap.put(attributes.getNamedItem("name").getTextContent(),
					parseRelation(Integer.parseInt(attributes.getNamedItem(
							"arity").getTextContent()), "conflicts"
							.equals(attributes.getNamedItem("semantics")
									.getTextContent()), Integer
							.parseInt(attributes.getNamedItem("nbTuples")
									.getTextContent()), relationNode
							.getTextContent()));
		}
		return relationMap;
	}

	/**
	 * Parse a relation to generate a CSPOM Extension.
	 * 
	 * @param arity
	 *            Arity of the relation.
	 * @param init
	 *            Whether tuples are initially true or false (resp. conflicts or
	 *            supports semantic).
	 * @param nbTuples
	 *            Number of tuples to be parsed.
	 * @param string
	 *            The relation to parse. Format is "a b c...|d e f...|..."
	 * @return The parsed extension
	 * @throws XCSPParseException
	 *             If the relation could not be read or is inconsistent with
	 *             given arity or nbTuples.
	 */
	private static Extension<Number> parseRelation(final int arity,
			final boolean init, final int nbTuples, final String string)
			throws XCSPParseException {

		final Extension<Number> extension = new Extension<Number>(arity, init);

		final String[] tupleList = string.split("\\|");

		if (tupleList.length != nbTuples) {
			throw new XCSPParseException("Inconsistent number of Tuples ("
					+ tupleList.length + " /= " + nbTuples + ") in " + string);
		}

		for (String parsedTuple : tupleList) {
			final String[] valueList = parsedTuple.trim().split(" +");

			if (valueList.length != arity) {
				throw new XCSPParseException("Incorrect arity ("
						+ valueList.length + " /= " + arity + ") in "
						+ parsedTuple.trim());
			}

			final Number[] tuple = new Number[arity];
			for (int j = arity; --j >= 0;) {
				tuple[j] = Integer.parseInt(valueList[j]);
			}
			extension.addTuple(tuple);

		}
		return extension;
	}

	/**
	 * Parse predicates defined in the given XCSP document and return them as a
	 * map. Keys are the predicates' names.
	 * 
	 * @param doc
	 *            XCSP document
	 * @return A map containing parsed predicates.
	 * @throws XCSPParseException
	 *             If a predicates does not contains both <parameters> and
	 *             <functional> parts.
	 */
	private static Map<String, Predicate> parsePredicates(final Document doc)
			throws XCSPParseException {
		final Map<String, Predicate> predicateMap = new HashMap<String, Predicate>();
		final NodeList predicates = doc.getElementsByTagName("predicate");
		for (int i = predicates.getLength(); --i >= 0;) {
			final Node predicateNode = predicates.item(i);
			final String name = predicateNode.getAttributes().getNamedItem(
					"name").getTextContent();

			try {
				predicateMap.put(name, new Predicate(PARAMS_EXPR
						.evaluate(predicateNode), FUNC_EXPR
						.evaluate(predicateNode)));
			} catch (XPathExpressionException e) {
				throw new XCSPParseException(
						"Could not read predicate " + name, e);
			}
		}

		return predicateMap;
	}

	/**
	 * Parse constraints, defined either by relations or predicates, and add
	 * them to the CSPOM problem.
	 * 
	 * @param relations
	 *            Relations constraints may use.
	 * @param predicates
	 *            Predicates constraints may use.
	 * @param doc
	 *            XCSP document.
	 * @throws XCSPParseException
	 *             If a relation or predicate could not be found or applied.
	 */
	private void parseConstraints(final Map<String, Extension<?>> relations,
			final Map<String, Predicate> predicates, final Document doc)
			throws XCSPParseException {
		final NodeList constraints = doc.getElementsByTagName("constraint");
		for (int i = constraints.getLength(); --i >= 0;) {
			final Node constraintNode = constraints.item(i);
			final NamedNodeMap attributes = constraintNode.getAttributes();
			addConstraint(attributes.getNamedItem("name").getTextContent(),
					attributes.getNamedItem("scope").getTextContent(),
					constraintNode.getTextContent(), attributes.getNamedItem(
							"reference").getTextContent(), predicates,
					relations);

		}

	}

	/**
	 * Parse and adds the given constraint to the problem.
	 * 
	 * @param name
	 *            Constraint name
	 * @param varNames
	 *            Scope of the constraint (variable names separated by spaces)
	 * @param parameters
	 *            Parameters of the constraint
	 * @param reference
	 *            Reference of the relation or predicate
	 * @param predicates
	 *            Map of predicates
	 * @param relations
	 *            Map of relations
	 * @throws XCSPParseException
	 *             If a variable, relation or predicate could not be found or
	 *             applied.
	 */
	private void addConstraint(final String name, final String varNames,
			final String parameters, final String reference,
			final Map<String, Predicate> predicates,
			final Map<String, Extension<?>> relations)
			throws XCSPParseException {
		final String[] scopeList = varNames.split(" +");
		final CSPOMVariable[] scope = new CSPOMVariable[scopeList.length];
		for (int i = 0; i < scopeList.length; i++) {
			scope[i] = problem.getVariable(scopeList[i]);
			if (scope[i] == null) {
				throw new XCSPParseException("Could not find variable "
						+ scopeList[i] + " from the scope of " + name);
			}
		}

		if (reference.startsWith("global:")) {
			final StringBuilder stb = new StringBuilder(reference.substring(7));
			stb.append("(").append(scope[0]);
			for (int i = 1; i < scope.length; i++) {
				stb.append(", ").append(scope[i]);
			}
			stb.append(")");
			try {
				problem.ctr(stb.toString());
			} catch (PredicateParseException e) {
				throw new XCSPParseException("Error parsing constraint "
						+ stb.toString(), e);
			}
			return;
		}

		final Extension<?> extension = relations.get(reference);
		if (extension != null) {
			problem.addConstraint(new ExtensionConstraint(name, extension,
					scope));
			return;
		}

		final Predicate predicate = predicates.get(reference);
		if (predicate == null) {
			throw new XCSPParseException("Unknown reference " + reference);
		}

		try {
			problem.ctr(predicate.applyParameters(parameters, scope));
		} catch (PredicateParseException e) {
			throw new XCSPParseException("Error parsing predicate " + predicate
					+ " with constraint parameters " + parameters, e);
		}
	}
}
