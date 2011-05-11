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

import com.google.common.base.Joiner;

import scala.collection.JavaConversions;
import scala.collection.immutable.Set;

import cspom.CSPOM;
import cspom.CSPParseException;
import cspom.compiler.PredicateParseException;
import cspom.extension.ExtensionConstraint;
import cspom.extension.Relation;
import cspom.variable.CSPOMDomain;
import cspom.variable.CSPOMVariable;
import cspom.variable.ExtensiveDomain;
import cspom.variable.IntInterval;

/**
 * This class implements an XCSP 2.0 parser.
 *
 * @author vion
 */
final class XCSPParser(private val problem: CSPOM) {

  /**
   * Append the XCSP data provided by the InputStream to the given CSPOM
   * problem.
   *
   * @param is
   *            The source of the XCSP data
   * @throws CSPParseException
   *             Thrown if is contains invalid data
   * @throws IOException
   *             Thrown if the data could not be read
   */
  @throws(classOf[CSPParseException])
  @throws(classOf[IOException])
  def parse(is: InputStream) {

    val db = DocumentBuilderFactory.newInstance().newDocumentBuilder();

    val document =
      try {
        db.parse(is);
      } catch {
        case e: SAXParseException =>
          throw new CSPParseException(e, e.getLineNumber());
        case e => throw e;
      }

    val domains = XCSPParser.parseDomains(document);
    parseVariables(domains, document);
//    val relations = parseRelations(document);
//    val predicates = parsePredicates(document);
//    parseConstraints(relations, predicates, document);
  }

  /**
   * Parse variables and add them to the problem.
   *
   * @param domains
   *            Domains to use for variables.
   * @param doc
   *            XCSP document
   * @throws CSPParseException
   *             If two variables have the same name.
   */
  @throws(classOf[CSPParseException])
  private def parseVariables(domains: Map[String, CSPOMDomain[java.lang.Integer]], doc: Document) {
    //        final NodeList variables = doc.getElementsByTagName("variable");
    //        for (int i = 0; i < variables.getLength(); i++) {
    //            final NamedNodeMap variableAttributes = variables.item(i)
    //                    .getAttributes();
    //            final String name = variableAttributes.getNamedItem("name")
    //                    .getTextContent();
    //            try {
    //                problem.addVariable(new CSPOMVariable<Integer>(name, domains
    //                        .get(variableAttributes.getNamedItem("domain")
    //                                .getTextContent()), false));
    //            } catch (IllegalStateException e) {
    //                throw new CSPParseException("Could not add variable " + name, e);
    //            }
    //
    //        }
  }
  //
  //    /**
  //     * Parse extension relations and return them as a map. Keys are the
  //     * relations' names.
  //     * 
  //     * @param doc
  //     *            XCSP document
  //     * @return The relations.
  //     * @throws CSPParseException
  //     *             If there was an error parsing a relation.
  //     */
  //    private static Map<String, Extension> parseRelations(final Document doc)
  //            throws CSPParseException {
  //        final Map<String, Extension> relationMap = new HashMap<String, Extension>();
  //        final NodeList relations = doc.getElementsByTagName("relation");
  //        for (int i = relations.getLength(); --i >= 0;) {
  //            final Node relationNode = relations.item(i);
  //            final NamedNodeMap attributes = relationNode.getAttributes();
  //
  //            relationMap.put(
  //                    attributes.getNamedItem("name").getTextContent(),
  //                    parseRelation(Integer.parseInt(attributes.getNamedItem(
  //                            "arity").getTextContent()), "conflicts"
  //                            .equals(attributes.getNamedItem("semantics")
  //                                    .getTextContent()), Integer
  //                            .parseInt(attributes.getNamedItem("nbTuples")
  //                                    .getTextContent()), relationNode
  //                            .getTextContent()));
  //        }
  //        return relationMap;
  //    }
  //
  //    /**
  //     * Parse a relation to generate a CSPOM Extension.
  //     * 
  //     * @param arity
  //     *            Arity of the relation.
  //     * @param init
  //     *            Whether tuples are initially true or false (resp. conflicts or
  //     *            supports semantic).
  //     * @param nbTuples
  //     *            Number of tuples to be parsed.
  //     * @param string
  //     *            The relation to parse. Format is "a b c...|d e f...|..."
  //     * @return The parsed extension
  //     * @throws CSPParseException
  //     *             If the relation could not be read or is inconsistent with
  //     *             given arity or nbTuples.
  //     */
  //    private static Extension parseRelation(final int arity, final boolean init,
  //            final int nbTuples, final String string) throws CSPParseException {
  //
  //        final Extension extension = new Extension(init, new Relation(arity));
  //        final String[] tupleList;
  //        if ("".equals(string.trim())) {
  //            tupleList = new String[0];
  //        } else {
  //            tupleList = string.split("\\|");
  //        }
  //
  //        if (tupleList.length != nbTuples) {
  //            throw new CSPParseException("Inconsistent number of Tuples ("
  //                    + tupleList.length + " /= " + nbTuples + ") in " + string);
  //        }
  //
  //        for (String parsedTuple : tupleList) {
  //            final String[] valueList = parsedTuple.trim().split(" +");
  //
  //            if (valueList.length != arity) {
  //                throw new CSPParseException("Incorrect arity ("
  //                        + valueList.length + " /= " + arity + ") in "
  //                        + parsedTuple.trim());
  //            }
  //
  //            final int[] tuple = new int[arity];
  //            for (int j = arity; --j >= 0;) {
  //                tuple[j] = Integer.parseInt(valueList[j]);
  //            }
  //            extension.relation.addTuple(tuple);
  //
  //        }
  //        return extension;
  //    }
  //
  //    /**
  //     * Parse predicates defined in the given XCSP document and return them as a
  //     * map. Keys are the predicates' names.
  //     * 
  //     * @param doc
  //     *            XCSP document
  //     * @return A map containing parsed predicates.
  //     * @throws CSPParseException
  //     *             If a predicates does not contains both <parameters> and
  //     *             <functional> parts.
  //     */
  //    private static Map<String, Predicate> parsePredicates(final Document doc)
  //            throws CSPParseException {
  //        final Map<String, Predicate> predicateMap = new HashMap<String, Predicate>();
  //        final NodeList predicates = doc.getElementsByTagName("predicate");
  //        for (int i = predicates.getLength(); --i >= 0;) {
  //            final Node predicateNode = predicates.item(i);
  //            final String name = predicateNode.getAttributes()
  //                    .getNamedItem("name").getTextContent();
  //
  //            try {
  //                predicateMap.put(name,
  //                        new Predicate(PARAMS_EXPR.evaluate(predicateNode),
  //                                FUNC_EXPR.evaluate(predicateNode)));
  //            } catch (XPathExpressionException e) {
  //                throw new CSPParseException("Could not read predicate " + name,
  //                        e);
  //            }
  //        }
  //
  //        return predicateMap;
  //    }
  //
  //    /**
  //     * Parse constraints, defined either by relations or predicates, and add
  //     * them to the CSPOM problem.
  //     * 
  //     * @param relations
  //     *            Relations constraints may use.
  //     * @param predicates
  //     *            Predicates constraints may use.
  //     * @param doc
  //     *            XCSP document.
  //     * @throws CSPParseException
  //     *             If a relation or predicate could not be found or applied.
  //     */
  //    private void parseConstraints(final Map<String, Extension> relations,
  //            final Map<String, Predicate> predicates, final Document doc)
  //            throws CSPParseException {
  //        final NodeList constraints = doc.getElementsByTagName("constraint");
  //        for (int i = constraints.getLength(); --i >= 0;) {
  //            final Node constraintNode = constraints.item(i);
  //            final NamedNodeMap attributes = constraintNode.getAttributes();
  //            addConstraint(attributes.getNamedItem("name").getTextContent(),
  //                    attributes.getNamedItem("scope").getTextContent(),
  //                    constraintNode.getTextContent(),
  //                    attributes.getNamedItem("reference").getTextContent(),
  //                    predicates, relations);
  //
  //        }
  //
  //    }
  //
  //    /**
  //     * Parse and adds the given constraint to the problem.
  //     * 
  //     * @param name
  //     *            Constraint name
  //     * @param varNames
  //     *            Scope of the constraint (variable names separated by spaces)
  //     * @param parameters
  //     *            Parameters of the constraint
  //     * @param reference
  //     *            Reference of the relation or predicate
  //     * @param predicates
  //     *            Map of predicates
  //     * @param relations
  //     *            Map of relations
  //     * @throws CSPParseException
  //     *             If a variable, relation or predicate could not be found or
  //     *             applied.
  //     */
  //    private void addConstraint(final String name, final String varNames,
  //            final String parameters, final String reference,
  //            final Map<String, Predicate> predicates,
  //            final Map<String, Extension> relations) throws CSPParseException {
  //        final List<CSPOMVariable<?>> scope = new ArrayList<CSPOMVariable<?>>();
  //        for (String s : varNames.split(" +")) {
  //            final CSPOMVariable<?> variable = problem.variable(s);
  //            if (variable == null) {
  //                throw new CSPParseException("Could not find variable " + s
  //                        + " from the scope of " + name);
  //            }
  //            scope.add(variable);
  //        }
  //
  //        if (reference.startsWith("global:")) {
  //            final StringBuilder stb = new StringBuilder(reference.substring(7));
  //            stb.append("(");
  //            Joiner.on(", ").appendTo(stb, scope);
  //            stb.append(")");
  //            try {
  //                problem.ctr(stb.toString());
  //            } catch (PredicateParseException e) {
  //                throw new CSPParseException("Error parsing constraint "
  //                        + stb.toString(), e);
  //            }
  //            return;
  //        }
  //
  //        final Extension extension = relations.get(reference);
  //        if (extension != null) {
  //            problem.addConstraint(new ExtensionConstraint(extension.relation,
  //                    extension.init, JavaConversions.asScalaBuffer(scope)));
  //            return;
  //        }
  //
  //        final Predicate predicate = predicates.get(reference);
  //        if (predicate == null) {
  //            throw new CSPParseException("Unknown reference " + reference);
  //        }
  //
  //        try {
  //            problem.ctr(predicate.applyParameters(parameters, scope));
  //        } catch (PredicateParseException e) {
  //            throw new CSPParseException("Error parsing predicate " + predicate
  //                    + " with constraint parameters " + parameters, e);
  //        }
  //    }
  //
  //    private static class Extension {
  //        final boolean init;
  //        final Relation relation;
  //
  //        public Extension(final boolean init, final Relation relation) {
  //            this.init = init;
  //            this.relation = relation;
  //        }
  //    }
}

object XCSPParser {

  private val xpath = XPathFactory.newInstance().newXPath();

  /**
   * XPath expression to retrieve predicate parameters.
   */
  val PARAMS_EXPR = xpath.compile("parameters");

  /**
   * XPath expression to retrieve predicate functional expression.
   */
  val FUNC_EXPR = xpath.compile("expression/functional");

  /**
   * Parse domains described in the document.
   *
   * @param doc
   *            The document
   * @return A map of all domains described in the document
   */
  def parseDomains(doc: Document) = {
    val domainMap = new HashMap[String, CSPOMDomain[java.lang.Integer]];
    val domains = doc.getElementsByTagName("domain");
    for (i <- 0 to domains.getLength - 1) {
      val domainNode = domains.item(i);
      domainMap.put(domainNode.getAttributes.getNamedItem("name").getTextContent,
        CSPOMDomain.valueOf(domainNode.getTextContent));

    }

    domainMap;
  }

}