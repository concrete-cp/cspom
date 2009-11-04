package cspom;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.GZIPInputStream;

import javax.script.ScriptException;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.apache.tools.bzip2.CBZip2InputStream;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.XMLReader;

import cspom.compiler.ConstraintCompiler;
import cspom.constraint.CSPOMConstraint;
import cspom.variable.CSPOMVariable;
import cspom.xcsp.ProblemHandler;

/**
 * 
 * Problem is the central class of the Constraint Satisfaction Problem Object
 * Model. You can create a problem from scratch by instantiating this class and
 * then using the addConstraint and addVariable classes. Most usually, the
 * Problem.load method is used in order to create a Problem object from a file.
 * <p>
 * The Problem class adheres to the following definition :
 * 
 * A CSP is defined as a pair (X, C), X being a finite set of variables, and C a
 * finite set of constraints.
 * 
 * A domain, that is a finite set of values is associated to each variable x in
 * X. Each constraint involves a finite set of variables (its scope) and defines
 * a set of allowed and forbidden instantiations of these variables.
 * 
 * 
 * 
 * @author Julien Vion
 * @see CSPOMConstraint
 * @see CSPOMVariable
 * 
 */
public class CSPOM {
	public final static String VERSION;

	static {
		Matcher matcher = Pattern.compile("Rev:\\ (\\d+)").matcher(
				"$Rev$");
		matcher.find();
		VERSION = matcher.group(1);
	}

	private final List<CSPOMVariable> variableList;

	private final Map<String, CSPOMVariable> variableMap;

	private final Collection<CSPOMConstraint> constraints;

	private final static Logger logger = Logger.getLogger(CSPOM.class
			.getSimpleName());

	private ConstraintCompiler constraintCompiler;

	/**
	 * Creates an empty problem, without any initial variables nor constraints.
	 */
	public CSPOM() {
		variableList = new LinkedList<CSPOMVariable>();
		variableMap = new HashMap<String, CSPOMVariable>();
		constraints = new ArrayList<CSPOMConstraint>();
	}

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

	public static CSPOM load(final String string) throws FileNotFoundException,
			ParseException, IOException {
		final URI uri;
		try {
			uri = new URI(string);
		} catch (URISyntaxException e) {
			throw new ParseException(e.toString(), 0);
		}

		if (uri.getScheme() == null) {
			return load(new URL("file://" + uri));
		}

		return load(uri.toURL());
	}

	public static CSPOM load(final URL url) throws ParseException,
			FileNotFoundException, IOException {
		final CSPOM problem = new CSPOM();
		final InputStream problemIS = problemInputStream(url);
		final SAXParserFactory saxParserFactory = SAXParserFactory
				.newInstance();
		final SAXParser saxParser;
		final XMLReader reader;

		try {
			saxParser = saxParserFactory.newSAXParser();
			reader = saxParser.getXMLReader();
		} catch (ParserConfigurationException e) {
			throw new ParseException(e.toString(), 0);
		} catch (SAXException e) {
			throw new ParseException(e.toString(), 0);
		}

		final ProblemHandler handler = new ProblemHandler(problem);

		reader.setContentHandler(handler);

		try {
			reader.parse(new InputSource(problemIS));
		} catch (SAXParseException e) {
			logger.throwing(CSPOM.class.getSimpleName(), "load", e);
			throw new IllegalStateException(e);
			// throw new ParseException("line " + e.getLineNumber() + ": "
			// + e.toString(), e.getLineNumber());
		} catch (SAXException e) {
			throw new ParseException(e.toString(), 0);
		}
		//
		// problem.variables.addAll(handler.getVariables());
		// problem.constraints.addAll(handler.getConstraints());

		return problem;
	}

	public List<CSPOMVariable> getVariables() {
		return variableList;
	}

	public void addVariable(CSPOMVariable variable)
			throws DuplicateVariableException {
		if (variableMap.put(variable.getName(), variable) != null) {
			throw new DuplicateVariableException();
		}
		variableList.add(variable);

	}

	public void addConstraint(CSPOMConstraint constraint) {
		constraints.add(constraint);
	}

	public Collection<CSPOMConstraint> getConstraints() {
		return constraints;
	}

	public Collection<CSPOMConstraint> checkSolution(final List<Number> solution)
			throws ScriptException {
		final Collection<CSPOMConstraint> falsified = new ArrayList<CSPOMConstraint>();
		for (CSPOMConstraint c : constraints) {
			final Number[] values = new Number[c.getArity()];
			for (int i = c.getArity(); --i >= 0;) {
				values[i] = solution.get(variableList.indexOf(c.getScope()[i]));
			}
			if (!c.evaluate(values)) {
				falsified.add(c);
			}
		}
		logger.info("Checked " + solution + " : " + falsified.size());
		return falsified;
	}

	public CSPOMVariable var(int lb, int ub) {
		final CSPOMVariable variable = new CSPOMVariable(lb, ub);
		try {
			addVariable(variable);
		} catch (DuplicateVariableException e) {
			throw new IllegalStateException();
		}
		return variable;
	}

	public CSPOMVariable var(String name, int lb, int ub)
			throws DuplicateVariableException {
		final CSPOMVariable variable = new CSPOMVariable(name, lb, ub);
		addVariable(variable);
		return variable;
	}

	public void ctr(String string) throws ParseException {
		if (constraintCompiler == null) {
			constraintCompiler = new ConstraintCompiler(this);
		}
		merge(constraintCompiler.split(string));
	}

	public CSPOMVariable getVariable(String variableName) {
		return variableMap.get(variableName);
	}

	private void merge(CSPOM problem) {
		for (CSPOMVariable v : problem.variableList) {
			if (!variableMap.containsValue(v)) {
				try {
					addVariable(v);
				} catch (DuplicateVariableException e) {
					throw new IllegalStateException(e);
				}
			}
		}

		constraints.addAll(problem.constraints);
	}
	
	public String toString() {
		final StringBuilder stb = new StringBuilder();
		for (CSPOMVariable v : variableList) {
			stb.append(v).append('\n');
		}
		for (CSPOMConstraint c : constraints) {
			stb.append(c).append('\n');
		}
		return stb.toString();
	}

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
				stb.append("source \"").append(c.getScope()[0]).append("\"\n");
				stb.append("target \"").append(c.getScope()[1]).append("\"\n");
				stb.append("label \"").append(c.getDescription())
						.append("\"\n");
				stb.append("]\n");
			}
		}
		stb.append("]\n");

		return stb.toString();
	}
}
