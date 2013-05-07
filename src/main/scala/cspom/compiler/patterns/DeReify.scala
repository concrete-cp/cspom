package cspom.compiler.patterns;

import cspom.constraint.{ GeneralConstraint, FunctionalConstraint, CSPOMConstraint }
import cspom.variable.TrueDomain
import cspom.CSPOM
import scala.collection.mutable.Queue
import cspom.constraint.Predicate

final class DeReify(
  private val problem: CSPOM,
  private val constraints: Queue[CSPOMConstraint]) extends ConstraintCompiler {

  override def compileFunctional(fc: FunctionalConstraint) = {
    if (fc.result.domainOption.map(_ == TrueDomain).getOrElse(false)) {
      problem.removeConstraint(fc);
      val newConstraint = new GeneralConstraint(
        Predicate(fc.predicate.function, fc.predicate.parameters), fc.arguments);
      problem.addConstraint(newConstraint);
      // constraints.enqueue(newConstraint);
      true
    } else false

  }

}
