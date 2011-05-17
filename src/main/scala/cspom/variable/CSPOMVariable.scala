package cspom.variable;

import cspom.constraint.CSPOMConstraint


/**
 * This class defines and implements CSP variables.
 *
 * @author vion
 *
 */
final class CSPOMVariable(
  val name: String,
  var domain: CSPOMDomain[Any],
  val auxiliary: Boolean) {

  var constraints: Set[CSPOMConstraint] = Set.empty

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
    constraints += constraint;
  }

  def removeConstraint(constraint: CSPOMConstraint): Unit = {
    assume(constraints contains constraint, this + " is not in " + constraint + "'s scope");
    constraints -= constraint
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
   * Constructs a new variable with singleton (constant) domain.
   *
   * @param <T>
   *            The type of the constant.
   * @param constant
   *            The unique value of the domain.
   */
  def constant[T](constant: T) =
    new CSPOMVariable(VariableNameGenerator.generate, new Constant[T](constant), true)

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
    new CSPOMVariable(name, new IntInterval(lb, ub), false);

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
  def of[T](values: T*) = ofSeq(values = values)

  def of[T](name: String, values: T*) = ofSeq(name, values)

  def ofSeq[T](name: String = VariableNameGenerator.generate, values: Seq[T]) =
    new CSPOMVariable(name, new ExtensiveDomain[T](values), false)

  def ofBool(name: String = VariableNameGenerator.generate, value: Boolean) =
    new CSPOMVariable(name, BooleanDomain.valueOf(value), false);

  def bool(name: String = VariableNameGenerator.generate) =
    new CSPOMVariable(name, UnknownBooleanDomain, false)

  def aux() = new CSPOMVariable(VariableNameGenerator.generate, null, true)
}