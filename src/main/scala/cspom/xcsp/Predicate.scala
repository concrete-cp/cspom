package cspom.xcsp;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.ListIterator;
import cspom.compiler.PredicateParseException;
import cspom.compiler.PredicateScanner;
import cspom.variable.CSPOMVariable;

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
  val parameters = parametersString.trim.split(" +").grouped(2).map { p => p(1) } toSeq

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
  @throws(classOf[PredicateParseException])
  def applyParameters(constraintParameters: String,
    scope: Seq[CSPOMVariable[_]]) = {
    val stringParameters = constraintParameters.trim.split(" +");

    if (stringParameters.length != this.parameters.size) {
      throw new PredicateParseException("Incorrect parameter count");
    }

    var applyied = expression;
    for (p <- stringParameters.zip(this.parameters)) {
      controlParameter(p._1, scope);
      applyied = applyied.replaceAll(p._2, p._1);
    }

    applyied;
  }

  /**
   * Controls whether the given parameter is valid (either an integer constant
   * or an existing variable).
   *
   * @param parameter
   *            The parameter.
   * @param scope
   *            An array of variable to validate the parameter against.
   * @throws PredicateParseException
   *             If the given parameter is invalid.
   */
  private def controlParameter(parameter: String, scope: Seq[CSPOMVariable[_]]) {
    if (PredicateScanner.INTEGER.matcher(parameter).matches()) {
      return ;
    }
    if (PredicateScanner.IDENTIFIER.matcher(parameter).matches()) {
      if (!scope.map { v => v.name }.contains(parameter)) {
        throw new PredicateParseException("Could not find variable "
          + parameter + " in " + scope);
      }
      return ;
    }
    throw new PredicateParseException("Could not recognize " + parameter);

  }
}

/**
 * An enumeration to represent various native types for CSP. For now, only
 * integers are supported. Use the static decl(String) method to avoid Java
 * reserved keywords.
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

  //        /**
  //         * Registers the value type with its XCSP declaration.
  //         * 
  //         * @param declaration
  //         *            XCSP declaration of the value type
  //         */
  //        def this(final String declaration) {
  //            DECLARATIONS.put(declaration, this);
  //        }
  //
  //        /**
  //         * Obtain types through this method to avoid Java reserved keywords such
  //         * as "int".
  //         * 
  //         * @param declaration
  //         *            The XCSP type declaration
  //         * @return The corresponding ValueType enum element
  //         */
  //        public static ValueType decl(final String declaration) {
  //            final ValueType type = DECLARATIONS.get(declaration);
  //            if (type == null) {
  //                return valueOf(declaration);
  //            }
  //            return type;
  //        }

}
