package cspom.xcsp;

import cspom.extension.{ Relation, ExtensionConstraint }
import cspom.variable.{ CSPOMVariable, CSPOMDomain }
import cspom.{ CSPParseException, CSPOM }
import java.io.InputStream
import scala.xml.{ XML, NodeSeq }

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
  def parse(is: InputStream) {
    val document = XML.load(is)
    parseVariables(document);
    parseConstraints(document);
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
  private def parseVariables(doc: NodeSeq) {
    val domains = (doc \ "domains" \ "domain") map { node =>
      (node \ "@name").text -> CSPOMDomain.valueOf(node.text)
    } toMap

    for (node <- doc \ "variables" \ "variable") {
      val domain = (node \ "@domain").text
      val name = (node \ "@name").text
      domains.get(domain) match {
        case Some(d) =>
          try {
            problem.addVariable(new CSPOMVariable(name, d, false));
          } catch {
            case e =>
              throw new CSPParseException("Could not add variable " + name, e);
          }
        case None =>
          throw new CSPParseException("Could not find domain " + domain)
      }
    }
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
   * @throws CSPParseException
   *             If a relation or predicate could not be found or applied.
   */
  private def parseConstraints(doc: NodeSeq) {
    val relations = ((doc \ "relations" \ "relation") map { node =>
      (node \ "@name").text -> XCSPParser.parseRelation(
        (node \ "@arity").text.toInt,
        "conflicts" == (node \ "@semantics").text,
        (node \ "@nbTuples").text.toInt,
        node.text)
    }).toMap ++ ((doc \ "predicates" \ "predicate") map { node =>
      (node \ "@name").text -> new Predicate((node \ "parameters").text,
        (node \ "expression" \ "functional").text)
    }).toMap;

    for (node <- doc \ "constraints" \ "constraint") {
      addConstraint(
        (node \ "@name").text,
        (node \ "@scope").text,
        node.text,
        (node \ "@reference").text,
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
   * @param relations
   *            Map of relations
   */
  private def addConstraint(name: String, varNames: String,
    parameters: String, reference: String, relations: Map[String, AnyRef]) {

    val scope = varNames.split(" +") map { s =>
      problem.variable(s) match {
        case Some(v) => v
        case None =>
          throw new CSPParseException("Could not find variable " + s
            + " from the scope of " + name);
      }
    }

    if (reference startsWith "global:") {

      val constraint = reference.substring(7) + scope.mkString("(", ", ", ")")

      try {
        problem.ctr(constraint);
      } catch {
        case e =>
          throw new CSPParseException("Error parsing constraint " + constraint, e);
      }

    } else relations.get(reference) match {

      case Some(relation) => relation match {
        case extension: Extension =>
          problem.addConstraint(new ExtensionConstraint(
            extension.relation,
            extension.init,
            scope.toList));
        case predicate: Predicate =>
          try {
            problem.ctr(predicate.applyParameters(parameters, scope))
          } catch {
            case e =>
              throw new CSPParseException("Error parsing predicate " + predicate
                + " with constraint parameters " + parameters, e);
          }
        case _ => throw new CSPParseException("Unknown relation type")
      }

      case None =>
        throw new CSPParseException("Unknown reference " + reference);

    }
  }
}

case class Extension(val init: Boolean, val relation: Relation);

object XCSPParser {
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
   * @throws CSPParseException
   *             If the relation could not be read or is inconsistent with
   *             given arity or nbTuples.
   */
  def parseRelation(arity: Int, init: Boolean, nbTuples: Int, string: String): Extension = {
    val extension = new Extension(init, new Relation(arity));
    val tupleList: Array[String] = string.trim match {
      case "" => Array.empty
      case s => s.split("""\|""");
    }

    assume(tupleList.size == nbTuples, "Inconsistent number of Tuples ("
      + tupleList.length + " /= " + nbTuples + ") in " + string);

    for (parsedTuple <- tupleList) {
      val values = parsedTuple.trim.split(" +");

      assume(values.length == arity, "Incorrect arity (" + values.length
        + " /= " + arity + ") in " + parsedTuple.trim);

      extension.relation.add(values map { _.toInt });
    }
    return extension;
  }
}
