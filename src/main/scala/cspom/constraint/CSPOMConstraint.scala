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

  //    @Override
  //    public final Integer getPosition(final CSPOMVariable[_] variable) {
  //        return positions.get(variable);
  //    }

  def involves(variable: CSPOMVariable[_]) = scopeSet.contains(variable)

  final def getVariable(position: Int) = scope(position)

  override def equals(obj: Any): Boolean = obj match {
    case c: CSPOMConstraint =>
      scope == c.scope && description == c.description && parameters == c.parameters
    case _ => false
  }

  override def iterator = scope.iterator

  def replaceVar(which: CSPOMVariable[_], by: CSPOMVariable[_]);
  //    @Override
  //    public void replaceVar(final CSPOMVariable merged, final CSPOMVariable var) {
  //        int pos = scope.indexOf(merged);
  //        if (pos < 0) {
  //            throw new IllegalArgumentException(merged + " not in scope");
  //        }
  //        do {
  //            scope.set(pos, var);
  //            positions.remove(merged);
  //            positions.put(var, pos);
  //            pos = scope.indexOf(merged);
  //        } while (pos >= 0);
  //        computeHashCode();
  //    }
  //
  //    public final String getParameters() {
  //        return parameters;
  //    }
  //
  //    public static final Function<CSPOMConstraint, String> CONSTRAINT_DESCRIPTION = new Function<CSPOMConstraint, String>() {
  //        @Override
  //        public String apply(final CSPOMConstraint input) {
  //            return input.toString();
  //        }
  //    };
  //
}

object CSPOMConstraint {
  val CONSTRAINT_DESCRIPTION = new com.google.common.base.Function[CSPOMConstraint, String] {
    override def apply(input: CSPOMConstraint) = input.toString;
  }

  def matchesDescription(description: String) = Predicates.compose(Predicates.equalTo(description),
    CONSTRAINT_DESCRIPTION);

}

trait PermutableConstraint extends CSPOMConstraint {
  def standardize(scope: CSPOMVariable[_]*): PermutableConstraint
}