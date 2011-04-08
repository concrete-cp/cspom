package cspom.constraint

import com.google.common.base.Predicates
import cspom.variable.CSPOMVariable
abstract class CSPOMConstraint(
  val name: String = null,
  val description: String,
  val parameters: String,
  val scope: List[CSPOMVariable[_]]) extends Iterable[CSPOMVariable[_]] {

  require(!scope.isEmpty)

  val arity = scope.size
  override val hashCode = 961 * parameters.hashCode + 31 * scope.hashCode + description.hashCode
  val scopeSet = scope.toSet
  //TODO: val positions

  def this(name: String, description: String, parameters: String, scope: CSPOMVariable[_]*) =
    this(name, description, parameters, scope.toList);

  def this(desc: String, params: String, scp: CSPOMVariable[_]*) =
    this(description = desc, parameters = params, scope = scp.toList);

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

object CSPOMConstraint {
  val CONSTRAINT_DESCRIPTION = new com.google.common.base.Function[CSPOMConstraint, String] {
    override def apply(input: CSPOMConstraint) = input.toString;
  }

  def matchesDescription(description: String) = Predicates.compose(Predicates equalTo description,
    CONSTRAINT_DESCRIPTION);

}


