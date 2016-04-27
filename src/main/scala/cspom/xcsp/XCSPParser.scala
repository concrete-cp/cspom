package cspom.xcsp;

import java.io.InputStream

import scala.util.Try
import scala.util.parsing.input.CharSequenceReader
import scala.xml.Elem
import scala.xml.NodeSeq
import scala.xml.XML

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.CSPOMGoal
import cspom.CSPParseException
import cspom.extension.Relation
import cspom.util.Infinitable
import cspom.util.IntInterval
import cspom.util.RangeSet
import cspom.variable.IntExpression
import cspom.variable.SimpleExpression

/**
 * This class implements an XCSP 2.0 parser.
 *
 * @author vion
 */
final object XCSPParser extends CSPOM.Parser {

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
  def parseDomain(desc: String): RangeSet[Infinitable] = {
    desc.trim.split(" +").foldLeft(RangeSet.empty[Infinitable]) { (i, v) =>
      if (v.contains("..")) {
        i ++ parseRange(v)
      } else {
        i ++ IntInterval.singleton(v.trim.toInt)
      }
    }

  }

  def parseRange(interval: String): IntInterval = interval.trim().split("\\.\\.") match {
    case Array(a, b) => IntInterval(a.toInt, b.toInt)
    case _           => throw new NumberFormatException("Interval format must be a..b");
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
  def apply(is: InputStream): Try[CSPOM] = {
    Try(XML.load(is))
      .flatMap(XCSPParser(_))
  }

  def apply(document: Elem): Try[CSPOM] = Try {
    val declaredVariables = parseVariables(document);

    CSPOM { implicit cspom: CSPOM =>
      for ((name, variable) <- declaredVariables) {
        variable as name
      }

      parseConstraints(document, declaredVariables.toMap, cspom)

      CSPOM.goal {
        CSPOMGoal.Satisfy(Map("variables" -> declaredVariables.map(_._1)))
      }
    }

  }

  /**
   * Parse variables and domains
   * @param doc
   *            XCSP document
   */
  private def parseVariables(doc: NodeSeq): Seq[(String, SimpleExpression[Int])] = {
    val domains = (doc \ "domains" \ "domain")
      .map { node => (node \ "@name").text -> parseDomain(node.text) }
      .toMap

    (doc \ "variables" \ "variable").map { node =>

      val domain = domains((node \ "@domain").text)
      val name = (node \ "@name").text

      name -> IntExpression(domain)
    }

  }

  case class Extension(val init: Boolean, val relation: Relation[Int])

  /**
   * Parse constraints, defined either by relations or predicates.
   *
   * @param relations
   *            Relations constraints may use.
   * @param predicates
   *            Predicates constraints may use.
   * @param doc
   *            XCSP document.
   */
  private def parseConstraints(doc: NodeSeq, declaredVariables: Map[String, SimpleExpression[Int]], cspom: CSPOM) = {
    val relations = ((doc \ "relations" \ "relation") map { node =>
      (node \ "@name").text -> {
        val text = new CharSequenceReader(node.text) //new StringReader(node.text)
        val arity = (node \ "@arity").text.toInt
        val nbTuples = (node \ "@nbTuples").text.toInt
        val init = "conflicts" == (node \ "@semantics").text
        Extension(init, ConstraintParser.parseTable(text, arity, nbTuples))
      }

    }).toMap ++ ((doc \ "predicates" \ "predicate") map { node =>
      (node \ "@name").text -> new XCSPPredicate((node \ "parameters").text,
        (node \ "expression" \ "functional").text)
    }).toMap;

    val gen = for (
      node <- doc \ "constraints" \ "constraint"
    ) {
      genConstraint(
        (node \ "@name").text,
        (node \ "@scope").text,
        node.text,
        (node \ "@reference").text,
        relations,
        declaredVariables, cspom)
    }

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
                            declaredVariables: Map[String, SimpleExpression[Int]], cspom: CSPOM): Unit = {

    val scope = varNames.split(" +").iterator
      .map { s =>
        s -> declaredVariables.getOrElse(s, {
          throw new CSPParseException("Could not find variable " + s
            + " from the scope of " + name);
        })
      }
      .toSeq

    if (reference startsWith "global:") {

      val constraint = reference.substring(7) + scope.map(_._1).mkString("(", ", ", ")")

      try {
        ConstraintParser.split(constraint, declaredVariables, cspom);
      } catch {
        case e: Exception =>
          throw new CSPParseException(s"Error parsing constraint $constraint", e);
      }

    } else {
      relations.get(reference) match {

        case Some(extension: Extension) => cspom.ctr(
          CSPOMConstraint('extension)(scope.map(_._2): _*) withParam (
            "init" -> extension.init, "relation" -> extension.relation))

        case Some(predicate: XCSPPredicate) =>
          try {
            ConstraintParser.split(predicate.applyParameters(parameters), declaredVariables, cspom)
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

