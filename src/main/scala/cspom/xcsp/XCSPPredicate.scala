package cspom.xcsp;

import cspom.variable.CSPOMVariable
import scala.util.matching.Regex
import scala.MatchError
import sun.misc.Regexp
import scala.util.parsing.input.CharSequenceReader
import scala.util.parsing.combinator.RegexParsers

/**
 * This class is used to represent XCSP predicates.
 *
 * @author vion
 *
 */
final class XCSPPredicate(parametersString: String, expressionString: String) extends RegexParsers {
  def typ: Parser[String] = "int"

  def paramId: Parser[String] = """\p{javaJavaIdentifierStart}\p{javaJavaIdentifierPart}*""".r

  def parameter: Parser[(String, String)] = typ ~ paramId ^^ { x => x._2 -> x._1 }

  def parametersParser: Parser[Seq[(String, String)]] = rep1(parameter)

  /**
   * Maps parameters to their respective types.
   */
  val (types, parameters) = {
    parametersParser(new CharSequenceReader(parametersString)) match {
      case Success(params, n) => (params.toMap, params.map(_._1))
      case o => throw new IllegalArgumentException(o.toString)
    }

  }

  /**
   * Predicate expression.
   */
  val expression = expressionString.trim;

  override def toString = "(" + parameters + "): " + expression

  override def equals(obj: Any) = obj match {
    case p: XCSPPredicate => expression == p.expression
    case _ => false
  }

  override val hashCode = expression.hashCode

  /**
   * Apply constraints parameters to the predicate and obtain the modified
   * expression. Parameter names are mapped to variable names or constants.
   *
   * @param constraintParameters
   *            Constraint parameters
   * @param scope
   *            Scope of the constraint
   * @return The expression obtained by applying the given parameters to the
   *         predicate.
   * @throws PredicateParseException
   *             if the constraint parameters are incompatible with the
   *             predicate parameters.
   *
   */
  def applyParameters(constraintParameters: String) = {
    val stringParameters = constraintParameters.trim.split(" +");

    assume(stringParameters.length == this.parameters.size, "Incorrect parameter count");

    ConstraintParser.mapFunc((this.parameters zip stringParameters).toMap)(new CharSequenceReader(expression)).get

  }

}

/**
 * An enumeration to represent various native types for CSP. For now, only
 * integers are supported.
 *
 * @author vion
 *
 */
object ValueType extends Enumeration {
  type Enumeration = Value
  /**
   * Standard integer (int) type.
   */
  val integer = Value("int");
}
