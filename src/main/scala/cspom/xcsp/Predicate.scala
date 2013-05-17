package cspom.xcsp;

import cspom.variable.CSPOMVariable
import scala.util.matching.Regex
import scala.MatchError
import sun.misc.Regexp
import cspom.compiler.ConstraintParser

/**
 * This class is used to represent XCSP predicates.
 *
 * @author vion
 *
 */
final class Predicate(parametersString: String, expressionString: String) {
  // private static final Map<String, ValueType> DECLARATIONS = new HashMap<String, ValueType>();

  /**
   * Maps parameters to their respective types.
   */
  val types = parametersString.trim.split(" +").grouped(2).map { p => p(1) -> p(0) } toMap

  /**
   * Ordered list of parameters.
   */
  val parameters = parametersString.trim.split(" +").grouped(2).map { p => p(1) } toList

  /**
   * Predicate expression.
   */
  val expression = expressionString.trim;

  override def toString = "(" + parameters + "): " + expression

  override def equals(obj: Any) = obj match {
    case p: Predicate => expression == p.expression
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
  def applyParameters(constraintParameters: String,
    scope: Seq[CSPOMVariable]) = {
    val stringParameters = constraintParameters.trim.split(" +");

    assume(stringParameters.length == this.parameters.size, "Incorrect parameter count");

    var applyied = expression;
    for ((s, p) <- stringParameters.zip(this.parameters)) {

      assume(
        ConstraintParser.isInt(s) ||
          (ConstraintParser.isId(s) && scope.map { v => v.name }.contains(s)),
        s"Did not recognize $s")

      applyied = (s"""([^A-Za-z])($p)([^A-Za-z0-9])""").r.replaceAllIn(applyied, { m =>
        m.group(1) + p + m.group(3)
      })

    }

    applyied;
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
