package cspom.compiler.patterns;

import cspom.variable.TrueDomain
import cspom.constraint.{GeneralConstraint, FunctionalConstraint, CSPOMConstraint}
import cspom.CSPOM
import java.util.Deque

final class DeReify(
  private val problem: CSPOM,
  private val constraints: Deque[CSPOMConstraint]) extends ConstraintCompiler {

  override def compile(c: CSPOMConstraint) {
    c match {
      case fc: FunctionalConstraint if fc.result.domain == TrueDomain => {
        problem.removeConstraint(fc);
        val newConstraint = new GeneralConstraint(
          fc.description, fc.parameters, fc.arguments);
        problem.addConstraint(newConstraint);
        constraints.add(newConstraint);
      }
      case _ =>
    }

  }

}
