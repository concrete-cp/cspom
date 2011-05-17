package cspom.compiler.patterns

import cspom.constraint.{ CSPOMConstraint, GeneralConstraint }
import cspom.variable.{ TrueDomain, CSPOMVariable }
import cspom.CSPOM
import scala.collection.mutable.Queue

class RemoveAnd(val problem: CSPOM, val constraints: Queue[CSPOMConstraint]) extends ConstraintCompiler {
  override def compile(constraint: CSPOMConstraint) {
    constraint match {
      case andConstraint: GeneralConstraint if constraint.description == "and" => {
        for (v <- constraint.scope) {
          v.asInstanceOf[CSPOMVariable].domain = TrueDomain
          for (c <- v.constraints if c != constraint)
            constraints.enqueue(c)
        }
        problem.removeConstraint(constraint);
      }
      case _ =>

    }
  }
}
