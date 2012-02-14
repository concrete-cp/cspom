package cspom.compiler.patterns;

import cspom.constraint.{ GeneralConstraint, FunctionalConstraint, CSPOMConstraint }
import cspom.variable.TrueDomain
import cspom.CSPOM
import scala.collection.mutable.Queue
import cspom.constraint.Predicate

final class DeReify(
  private val problem: CSPOM,
  private val constraints: Queue[CSPOMConstraint]) extends ConstraintCompiler {

  override def compile(c: CSPOMConstraint) {
    c match {
      case fc: FunctionalConstraint if fc.result.domain == TrueDomain => {
        problem.removeConstraint(fc);
        val newConstraint = new GeneralConstraint(
          Predicate(fc.predicate.function, fc.predicate.parameters), fc.arguments);
        problem.addConstraint(newConstraint);
        constraints.enqueue(newConstraint);
      }
      case _ =>
    }

  }

}
