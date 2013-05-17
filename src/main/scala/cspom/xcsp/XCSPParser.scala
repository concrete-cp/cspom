package cspom.xcsp;

import java.io.InputStream
import scala.xml.NodeSeq
import scala.xml.XML
import cspom.extension.ExtensionConstraint
import cspom.variable.CSPOMDomain
import cspom.variable.CSPOMVariable
import cspom.CSPOM
import cspom.CSPParseException
import cspom.extension.LazyRelation
import cspom.extension.Relation
import java.io.StringReader
import cspom.variable.ProblemVar
import cspom.compiler.ConstraintParser

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
      val domain = domains((node \ "@domain").text)
      val name = (node \ "@name").text
      
      try {
        problem.addVariable(new ProblemVar(name, domain));
      } catch {
        case e: Exception =>
          throw new CSPParseException(s"Could not add variable $name", e);
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
      (node \ "@name").text -> {
        val text = new StringReader(node.text)
        val arity = (node \ "@arity").text.toInt
        val nbTuples = (node \ "@nbTuples").text.toInt
        val init = "conflicts" == (node \ "@semantics").text
        new Extension(init, new LazyRelation(text, arity, nbTuples))
      }

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
      problem.variable(s).getOrElse {
        throw new CSPParseException("Could not find variable " + s
          + " from the scope of " + name);
      }
    }

    if (reference startsWith "global:") {

      val constraint = reference.substring(7) + scope.mkString("(", ", ", ")")

      try {
        ConstraintParser.split(constraint, problem);
      } catch {
        case e: Exception =>
          throw new CSPParseException(s"Error parsing constraint $constraint", e);
      }

    } else {
      relations.get(reference) match {

        case Some(extension: Extension) => problem.addConstraint(new ExtensionConstraint(
          extension.relation,
          extension.init,
          scope.toList));
        case Some(predicate: Predicate) =>
          try {
            ConstraintParser.split(predicate.applyParameters(parameters, scope), problem)
          } catch {
            case e: Exception =>
              throw new CSPParseException("Error parsing predicate " + predicate
                + " with constraint parameters " + parameters, e);
          }
        case _ => throw new CSPParseException(s"Unknown relation type or reference: $reference")

      }
    }
  }
}

final class Extension(val init: Boolean, val relation: Relation)
