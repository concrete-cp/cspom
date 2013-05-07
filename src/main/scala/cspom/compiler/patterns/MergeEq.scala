package cspom.compiler.patterns;

import cspom.constraint.{ GeneralConstraint, CSPOMConstraint }
import cspom.variable.{ CSPOMVariable, CSPOMDomain }
import cspom.CSPOM
import scala.collection.mutable.Queue

/**
 * If given constraint is an all-equal constraint, merges and removes all
 * auxiliary variables.
 */
final class MergeEq(private val problem: CSPOM,
  private val constraints: Queue[CSPOMConstraint]) extends ConstraintCompiler {

  override def compileGeneral(c: GeneralConstraint) = {
    if (c.description == "eq") {
      (for (
        (auxVars, fullVars) <- Some(c.scope.partition { _.auxiliary });
        if (auxVars.nonEmpty)
      ) yield {

        problem.removeConstraint(c)

        /*
         * Generate a new all-equal constraint if more than one variable
         * remains.
         */
        if (fullVars.size > 1) {
          val newConstraint = new GeneralConstraint("eq", fullVars: _*);
          constraints.enqueue(newConstraint)
          problem.addConstraint(newConstraint)
        }

        /*
         * Update the constraints of the problem
         */
        val refVar = if (fullVars.isEmpty) auxVars.head else fullVars.head

        for (aux <- auxVars if aux != refVar) {
          merge(aux, refVar)
          for (c <- aux.constraints) constraints.enqueue(c)
        }
        true

      }) isDefined
    } else false
  }

  private def mergeDomain[T](d0: Option[CSPOMDomain[T]], d1: Option[CSPOMDomain[T]]) = {
    if (d0.isEmpty) { d1 }
    else if (d1.isEmpty) { d0 }
    else { Some(d0.get.intersect(d1.get)) }
  }

  private def merge(merged: CSPOMVariable, variable: CSPOMVariable) {
    assume(merged != variable)

    for (d <- mergeDomain(merged.domainOption, variable.domainOption)) {
      variable.domain = d
    }

    for (c <- merged.constraints) {
      problem.removeConstraint(c);
      problem.addConstraint(c.replacedVar(merged, variable));
    }
    problem.removeVariable(merged);
  }

}
