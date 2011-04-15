package cspom.constraint

import com.google.common.base.Predicate
import com.google.common.base.Predicates
import cspom.variable.CSPOMVariable
abstract class CSPOMConstraint(
  val description: String,
  val parameters: String) extends Iterable[CSPOMVariable[_]] {

  def scope: Seq[CSPOMVariable[_]]
  val arity = scope.size
  override val hashCode = {
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

  override def iterator = scope.iterator

  def replaceVar[T](which: CSPOMVariable[T], by: CSPOMVariable[T]);

  def evaluate(t: Any*): Boolean;
}
//
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

