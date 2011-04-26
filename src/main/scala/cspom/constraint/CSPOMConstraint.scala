package cspom.constraint

import _root_.cspom.variable.CSPOMVariable
abstract class CSPOMConstraint(
  val description: String,
  val parameters: String,
  val scope: Seq[CSPOMVariable[_]]) {

  val arity = scope.size
  override def hashCode = {
    var hash = 31 * scope.hashCode + description.hashCode
    if (parameters != null) {
      hash *= 31
      hash += parameters.hashCode
    }
    hash
  }

  val scopeSet = scope.toSet
  //TODO: val positions

  def involves(variable: CSPOMVariable[_]) = scopeSet.contains(variable)

  final def getVariable(position: Int) = scope(position)

  override def equals(obj: Any): Boolean = obj match {
    case c: CSPOMConstraint =>
      scope == c.scope && description == c.description && parameters == c.parameters
    case _ => false
  }

  def replacedVar[T](which: CSPOMVariable[T], by: CSPOMVariable[T]): CSPOMConstraint;

  def evaluate(t: Seq[_]): Boolean;
}
//object CSPOMConstraint {
//  val CONSTRAINT_DESCRIPTION = new com.google.common.base.Function[CSPOMConstraint, String] {
//    override def apply(input: CSPOMConstraint) = input.toString;
//  }
//
//  def matchesDescription(description: String): Predicate[CSPOMConstraint] =
//    Predicates.compose(Predicates.equalTo(description),
//      CONSTRAINT_DESCRIPTION);
//
//}

