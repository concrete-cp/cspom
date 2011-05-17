package cspom.extension
import cspom.variable.CSPOMVariable
import cspom.constraint.{ PermutableConstraint, CSPOMConstraint }

/**
 * A class that implements extension constraints, that it constraints defined by
 * a set of allowed or forbidden tuples.
 *
 * @author vion
 *
 * @param <T>
 */
final class ExtensionConstraint(
  val relation: Relation,
  val init: Boolean,
  scope: Seq[CSPOMVariable[Any]]) extends CSPOMConstraint("ext", null, scope) with PermutableConstraint {

  override def toString = super.toString + ": " + relation;

  override def evaluate(tuple: Seq[_]) = init ^ relation.contains(tuple)

  override val hashCode = 31 * super.hashCode + relation.hashCode

  override def equals(obj: Any) = obj match {
    case ec: ExtensionConstraint => super.equals(ec) && ec.relation == this.relation
    case _ => false
  }

  def standardize(newScope: Seq[CSPOMVariable[Any]]) = {
    assert(newScope.size == arity)
    new ExtensionConstraint(relation.permute(scope.map(v => newScope.indexOf(v))), init, newScope)
  }

  override def replacedVar[T >: Any](which: CSPOMVariable[T], by: CSPOMVariable[T]) = {
    new ExtensionConstraint(relation, init,
      scope map { v => if (v == which) by else v })
  }

}