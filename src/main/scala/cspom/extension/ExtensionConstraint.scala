package cspom.extension
import cspom.constraint.{ PermutableConstraint, CSPOMConstraint }
import cspom.variable.CSPOMVariable
import cspom.xcsp.Extension

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
  scope: Seq[CSPOMVariable])
  extends CSPOMConstraint("ext", null, scope) with PermutableConstraint {

  def this(relation: Relation, init: Boolean, scope: Array[CSPOMVariable]) = this(relation, init, scope.toSeq)

  override def toString = super.toString + ": " + relation;

  override def evaluate(tuple: Seq[_]) = init ^ relation.contains(tuple)

  override val hashCode = 31 * super.hashCode + relation.hashCode

  override def equals(obj: Any) = obj match {
    case ec: ExtensionConstraint => super.equals(ec) && ec.relation == this.relation
    case _ => false
  }

  def standardize(newScope: Seq[CSPOMVariable]) = {
    assert(newScope.size == arity)
    new ExtensionConstraint(relation.permute(scope.map(v => newScope.indexOf(v))), init, newScope.toList)
  }

  override def replacedVar(which: CSPOMVariable, by: CSPOMVariable) = {
    new ExtensionConstraint(relation, init,
      scope map { v => if (v == which) by else v })
  }

}