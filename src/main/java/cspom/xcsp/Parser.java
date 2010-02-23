package cspom.xcsp;

import java.io.IOException;
import java.io.InputStream;
import java.text.ParseException;
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
import cspom.variable.Domain;
import cspom.variable.ExtensiveDomain;
import cspom.variable.Interval;

/**
 * This class implements an XCSP 2.0 parser.
 * 
 * @author vion
 */
public final class Parser {

	private static final XPathExpression PARAMS_EXPR;
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

	private final CSPOM problem;

	/**
	 * Constructor.
	 */
	public Parser(CSPOM problem) {
		this.problem = problem;
	}

	/**
	 * Append the XCSP data provided by the InputStream to the given CSPOM
	 * problem.
	 * 
	 * @param is
	 *            The source of the XCSP data
	 * @param problem
	 *            The target CSPOM problem
	 * @throws ParseException
	 *             Thrown on invalid data
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

		final Map<String, Domain<?>> domains = parseDomains(document);
		parseVariables(domains, document);
		final Map<String, Extension> relations = parseRelations(document);
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
	private static Map<String, Domain<?>> parseDomains(final Document doc) {
		final Map<String, Domain<?>> domainMap = new HashMap<String, Domain<?>>();
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
	private static Domain<?> parseDomain(final String domain) {
		final String[] listOfValues = domain.trim().split(" +");

		if (listOfValues.length == 1 && listOfValues[0].contains("..")) {
			final String[] fromto = listOfValues[0].trim().split("\\.\\.");

			final int start = Integer.parseInt(fromto[0]);
			final int end = Integer.parseInt(fromto[1]);
			return new Interval<Integer>(start, end);

		}

		final List<Integer> values = new ArrayList<Integer>();

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

		return new ExtensiveDomain<Integer>(values);

	}

	/**
	 * @param problem
	 * @param domains
	 * @param doc
	 * @throws ParseException
	 */
	private void parseVariables(final Map<String, Domain<?>> domains,
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

	private static Map<String, Extension> parseRelations(final Document doc)
			throws XCSPParseException {
		final Map<String, Extension> relationMap = new HashMap<String, Extension>();
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

	private static Extension parseRelation(final int arity, final boolean init,
			final int nbTuples, final String string) throws XCSPParseException {

		final Extension extension = new Extension(arity, init);

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

	private void parseConstraints(final Map<String, Extension> relations,
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

	private void addConstraint(final String name, final String varNames,
			final String parameters, final String reference,
			final Map<String, Predicate> predicates,
			final Map<String, Extension> relations) throws XCSPParseException {
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

		final Extension extension = relations.get(reference);
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
