package cspom.compiler.patterns

import _root_.cspom.variable.CSPOMVariable
import scala.collection.JavaConversions
import _root_.cspom.variable.TrueDomain
import _root_.cspom.constraint.GeneralConstraint
import _root_.cspom.CSPOM
import _root_.cspom.constraint.CSPOMConstraint
import java.util.Deque
class RemoveAnd(val problem: CSPOM, val constraints: Deque[CSPOMConstraint]) extends ConstraintCompiler {
  override def compile(constraint: CSPOMConstraint) {
    constraint match {
      case andConstraint: GeneralConstraint if constraint.description == "and" => {
        for (v <- constraint.scope) {
          v.asInstanceOf[CSPOMVariable[Boolean]].domain = TrueDomain
          constraints.addAll(JavaConversions.asJavaCollection(
            v.constraints filter { _ != constraint }))
        }
        problem.removeConstraint(constraint);
      }

    }
  }
}
