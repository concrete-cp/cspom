package cspom.variable;

import cspom.constraint.CSPOMConstraint
import scala.collection.mutable.HashSet

/**
 * This class defines and implements CSP variables.
 *
 * @author vion
 *
 */
class CSPOMVariable[T](
  val name: String = VariableNameGenerator.generate,
  var domain: CSPOMDomain[T] = null,
  val auxiliary: Boolean = false) {

  val constraints = new HashSet[CSPOMConstraint]

  /**
   * Constructs a new variable with singleton (constant) domain.
   *
   * @param <T>
   *            The type of the constant.
   * @param constant
   *            The unique value of the domain.
   */
  def this(constant: T) {
    this(domain = new Constant[T](constant));
  }

  override def toString = domain match {
    case c: Constant[_] => c.toString
    case _ => name
  }

  /**
   * This method is used to register the given constraint in the set of
   * constraints involving this variable.
   *
   * @param constraint
   *            The constraint involving the variable.
   */
  def registerConstraint(constraint: CSPOMConstraint) {
    assume(constraint.involves(this), constraint + " does not involve " + this);
    constraints.add(constraint);
  }

  def removeConstraint(constraint: CSPOMConstraint): Unit = {
    assume(constraints.remove(constraint), this + " is not in "
      + constraint + "'s scope");
  }
}

object VariableNameGenerator {
  var unnamed = 0;

  /**
   * Generates an unique variable name.
   *
   * @return An unique variable name.
   */
  def generate = {
    val name = "_" + unnamed;
    unnamed += 1;
    name;
  }
}

object CSPOMVariable {

  /**
   * Constructs a new variable with a domain defined by lower
   * and upper bounds.
   *
   * @param <E>
   *            Type of bounds.
   * @param lB
   *            Lower bound of the domain
   * @param uB
   *            Upper bound of the domain
   */
  def ofInterval(name: String = VariableNameGenerator.generate, lb: Int, ub: Int) =
    new CSPOMVariable(name, new IntInterval(lb, ub));

  /**
   * Constructs a new variable with given name. Domain is defined by a list of
   * values.
   *
   * @param <T>
   *            Type of the values.
   * @param name
   *            Name of the variable.
   * @param values
   *            List of values defining the domain.
   */
  def of[T](values: T*) = ofList(values.toList)

  def of[T](name: String, values: T*) = ofList(values.toList)

  def ofList[T](values: List[T]) =
    new CSPOMVariable[T](domain = new ExtensiveDomain[T](values.toList))

  def ofList[T](name: String, values: List[T]) =
    new CSPOMVariable[T](name = name, domain = new ExtensiveDomain[T](values.toList))

  def ofBool(boolean: Boolean) =
    new CSPOMVariable[Boolean](domain = BooleanDomain.valueOf(boolean));

  def ofBool(name: String, boolean: Boolean) =
    new CSPOMVariable[Boolean](name = name, domain = BooleanDomain.valueOf(boolean));

  def bool() = new CSPOMVariable[Boolean](domain = UnknownBooleanDomain)

  def bool(name: String) =
    new CSPOMVariable[Boolean](name = name, domain = UnknownBooleanDomain)
}