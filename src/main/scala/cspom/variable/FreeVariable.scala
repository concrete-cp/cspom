package cspom.variable;

import cspom.CSPOMConstraint
import cspom.CSPOM
import scala.collection.mutable.HashMap

/**
 * This class defines and implements CSP variables.
 *
 * @author vion
 *
 */

class FreeVariable(params: Set[String]) extends CSPOMVariable(params) {
  def this(params: String*) = this(params.toSet)
  override def toString = s"free variable"
  def intersected(other: CSPOMExpression) = other
  def contains(that: CSPOMConstant) = true
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

