package cspom.xcsp;

import java.io.InputStream
import scala.xml.NodeSeq
import scala.xml.XML
import cspom.variable.CSPOMVariable
import cspom.CSPOM
import cspom.CSPParseException
import cspom.extension.LazyRelation
import cspom.extension.Relation
import java.io.StringReader
import cspom.variable.IntDomain
import cspom.variable.IntVariable
import scala.util.parsing.input.CharSequenceReader
import cspom.CSPOMConstraint
import cspom.variable.CSPOMSeq
import cspom.variable.IntExpression
import cspom.variable.IntInterval
import cspom.variable.IntConstant

/**
 * This class implements an XCSP 2.0 parser.
 *
 * @author vion
 */
final object XCSPParser {

  /**
   * Parse the given expression given as a String. Domains are usually sequence of
   * values separated by spaces such as "1 3 -4 5" or intervals in the format
   * "a..b". Sequences of values and intervals such as "1 3..10 18..30" are
   * allowed and converted to a sequence of values.
   *
   * @param domain
   *            The String domain to parse
   * @return The resulting Domain object
   */
  def parseDomain(desc: String): IntExpression = {
    val values: Seq[Int] = desc.trim.split(" +").flatMap { v =>
      if (v.contains("..")) {
        IntInterval.valueOf(v);
      } else {
        List(v.trim.toInt);
      }
    }

    if (values.size == 1) {
      IntConstant(values.head)
    } else {
      IntVariable.ofSeq(values)
    }

  }

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
  def parse(is: InputStream): (CSPOM, Map[Symbol, Any]) = {
    val document = XML.load(is)
    val declaredVariables = parseVariables(document);
    val (genVariables, constraints) = parseConstraints(document, declaredVariables.toMap);
    val problem = new CSPOM

    for ((name, variable) <- declaredVariables) {
      problem.nameExpression(variable, name)
    }
    //declaredVariables.values.foreach(problem.addVariable)
    //genVariables.foreach(problem.addVariable)
    constraints.foreach(problem.ctr)
    (problem, Map('variables -> declaredVariables.map(_._1)))

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
  private def parseVariables(doc: NodeSeq): Seq[(String, CSPOMVariable)] = {
    val domains = (doc \ "domains" \ "domain") map { node =>
      (node \ "@name").text -> IntDomain.valueOf(node.text)
    } toMap

    for (node <- doc \ "variables" \ "variable") yield {
      val domain = domains((node \ "@domain").text)
      val name = (node \ "@name").text

      name -> new IntVariable(domain)
      //      try {
      //        problem.addVariable(new IntVariable(name, domain));
      //      } catch {
      //        case e: Exception =>
      //          throw new CSPParseException(s"Could not add variable $name", e);
      //      }

    }

  }

  /**
   * Parse constraints, defined either by relations or predicates.
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
  private def parseConstraints(doc: NodeSeq, declaredVariables: Map[String, CSPOMVariable]) = {
    val relations = ((doc \ "relations" \ "relation") map { node =>
      (node \ "@name").text -> {
        val text = new CharSequenceReader(node.text) //new StringReader(node.text)
        val arity = (node \ "@arity").text.toInt
        val nbTuples = (node \ "@nbTuples").text.toInt
        val init = "conflicts" == (node \ "@semantics").text
        Extension(init, new LazyRelation(text, arity, nbTuples))
      }

    }).toMap ++ ((doc \ "predicates" \ "predicate") map { node =>
      (node \ "@name").text -> new XCSPPredicate((node \ "parameters").text,
        (node \ "expression" \ "functional").text)
    }).toMap;

    val gen = for (
      node <- doc \ "constraints" \ "constraint"
    ) yield genConstraint(
      (node \ "@name").text,
      (node \ "@scope").text,
      node.text,
      (node \ "@reference").text,
      relations,
      declaredVariables)

    val (genVars, genCons) = gen.unzip
    (genVars.flatten, genCons.flatten)

  }

  /**
   * Parse the given predicate and returns a sequence of auxiliary variables and constraints
   * required to represent it.
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
  private def genConstraint(name: String, varNames: String,
    parameters: String, reference: String, relations: Map[String, AnyRef],
    declaredVariables: Map[String, CSPOMVariable]): (Seq[CSPOMVariable], Seq[CSPOMConstraint]) = {

    val scope = varNames.split(" +") map { s =>
      s -> declaredVariables.getOrElse(s, {
        throw new CSPParseException("Could not find variable " + s
          + " from the scope of " + name);
      })
    }

    if (reference startsWith "global:") {

      val constraint = reference.substring(7) + scope.map(_._1).mkString("(", ", ", ")")

      try {
        ConstraintParser.split(constraint, declaredVariables);
      } catch {
        case e: Exception =>
          throw new CSPParseException(s"Error parsing constraint $constraint", e);
      }

    } else {
      relations.get(reference) match {

        case Some(extension: Extension) => (Seq(), Seq(new CSPOMConstraint(
          'extension, scope.map(_._2), Map("init" -> extension.init, "relation" -> extension.relation))))

        case Some(predicate: XCSPPredicate) =>
          try {
            ConstraintParser.split(predicate.applyParameters(parameters), declaredVariables)
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

final case class Extension(val init: Boolean, val relation: Relation)
