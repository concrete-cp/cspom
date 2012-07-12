package cspom.compiler.patterns;

import cspom.constraint.{ GeneralConstraint, CSPOMConstraint }
import cspom.variable.{ CSPOMVariable, CSPOMDomain }
import cspom.CSPOM
import scala.collection.mutable.Queue
import cspom.constraint.FunctionalConstraint

/**
 * If given constraint is an all-equal constraint, merges and removes all
 * auxiliary variables.
 */
final class MergeSame(private val problem: CSPOM,
  private val constraints: Queue[CSPOMConstraint]) extends ConstraintCompiler {

  override def compileFunctional(c: FunctionalConstraint) {
    for (
      same <- problem.functionalConstraints if (
        (same ne c) &&
        same.description == c.description &&
        same.arguments.sameElements(c.arguments))
    ) {
      problem.removeConstraint(c)
      val eqC = new GeneralConstraint("eq", c.result, same.result)
      problem.addConstraint(eqC)

      for (c <- c.result.constraints)
        constraints.enqueue(c)

      for (c <- same.result.constraints)
        constraints.enqueue(c)
    }
  }

}
