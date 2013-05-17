package cspom.variable;

import cspom.constraint.CSPOMConstraint
import cspom.constraint.FunctionalConstraint
import cspom.constraint.GeneralConstraint
import cspom.CSPOM

/**
 * This class defines and implements CSP variables.
 *
 * @author vion
 *
 */
abstract class CSPOMVariable(val name: String, var _domain: Option[CSPOMDomain[Any]]) {

  def auxiliary: Boolean

  var constraints: Set[CSPOMConstraint] = Set.empty

  def domain_=(d: CSPOMDomain[Any]) {
    require(_domain.isEmpty)
    _domain = Some(d)
  }

  def domain = _domain.get

  def domainOption = _domain

  def intersectDomains(d: CSPOMDomain[Any]) {
    _domain = _domain.map(_.intersect(d)).orElse(Some(d))
  }

  override def toString = {
    val n = domainOption match {
      case Some(c: Constant[_]) => c.toString
      case _ => name
    }

    if (auxiliary) s"$n aux" else n
  }

  def functionalConstraints = constraints.iterator
    .filter { _.isInstanceOf[FunctionalConstraint] }
    .map { _.asInstanceOf[FunctionalConstraint] }

  def generalConstraints = constraints.iterator
    .filter { _.isInstanceOf[GeneralConstraint] }
    .map { _.asInstanceOf[GeneralConstraint] }

  /**
   * This method is used to register the given constraint in the set of
   * constraints involving this variable.
   *
   * @param constraint
   *            The constraint involving the variable.
   */
  def registerConstraint(constraint: CSPOMConstraint) {
    assume(constraint.involves(this), s"$constraint does not involve $this");
    constraints += constraint;
  }

  def removeConstraint(constraint: CSPOMConstraint): Unit = {
    assume(constraints contains constraint, s"$this is not in $constraint's scope");
    constraints -= constraint
  }

  def is(name: String, scope: CSPOMVariable*)(implicit problem: CSPOM) {
    problem.addConstraint(new FunctionalConstraint(this, name, scope: _*))
  }

  def >(other: CSPOMVariable)(implicit problem: CSPOM) = problem.is("gt", this, other)

  def >=(other: CSPOMVariable)(implicit problem: CSPOM) = problem.is("ge", this, other)

  def <(other: CSPOMVariable)(implicit problem: CSPOM) = problem.is("lt", this, other)

  def <=(other: CSPOMVariable)(implicit problem: CSPOM) = problem.is("le", this, other)

  def ne(other: CSPOMVariable)(implicit problem: CSPOM) = problem.is("ne", this, other)

  def ≠(other: CSPOMVariable)(implicit problem: CSPOM) = ne(other)

  def ==(other: CSPOMVariable)(implicit problem: CSPOM) = problem.is("eq", this, other)

  def +(other: CSPOMVariable)(implicit problem: CSPOM) = problem.is("add", this, other)

  def -(other: CSPOMVariable)(implicit problem: CSPOM) = problem.is("sub", this, other)

  def *(other: CSPOMVariable)(implicit problem: CSPOM) = problem.is("mul", this, other)

  def /(other: CSPOMVariable)(implicit problem: CSPOM) = problem.is("div", this, other)

  def |(other: CSPOMVariable)(implicit problem: CSPOM) = problem.is("or", this, other)

  def &(other: CSPOMVariable)(implicit problem: CSPOM) = problem.is("and", this, other)
}

object VariableNameGenerator {
  var unnamed = 0;

  /**
   * Generates an unique variable name.
   *
   * @return An unique variable name.
   */
  def generate() = {
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
    new ProblemVar(VariableNameGenerator.generate, new Constant[T](constant))

  var intervals: Map[(Int, Int), IntInterval] = Map.empty

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
  def ofInterval(name: String = VariableNameGenerator.generate, lb: Int, ub: Int) = {
    val i = intervals.get(lb, ub) match {
      case Some(i) => i
      case None =>
        val i = new IntInterval(lb, ub)
        intervals += (lb, ub) -> i
        i

    }
    new ProblemVar(name, i);
  }

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

  def ofSeq[T](name: String = VariableNameGenerator.generate(), values: Seq[T]) =
    new ProblemVar(name, new ExtensiveDomain[T](values))

  def ofBool(name: String = VariableNameGenerator.generate(), value: Boolean) =
    new ProblemVar(name, BooleanDomain.valueOf(value));

  def bool(name: String = VariableNameGenerator.generate()) =
    new ProblemVar(name, UnknownBooleanDomain)

}