package cspom.compiler.patterns

import cspom.constraint.FunctionalConstraint
import cspom.constraint.CSPOMConstraint
import cspom.CSPOM
import cspom.constraint.GeneralConstraint
import scala.collection.mutable.Queue

import scala.util.control.Breaks._

/**
 * Transforms x = a \/ b, x \/ c \/ ... into a \/ b \/ c \/ ...
 */
final class NeqVec(
  private val problem: CSPOM,
  private val constraints: Queue[CSPOMConstraint]) extends ConstraintCompiler {

  override def compileFunctional(fc: FunctionalConstraint) {
    if ((fc.description == "ne" || fc.description == "nevec") &&
      fc.result.auxiliary) {

      breakable {
        for (
          orConstraint <- fc.result.generalConstraints if orConstraint.description == "or";
          orVariable <- orConstraint.scope;
          neConstraint <- orVariable.functionalConstraints if (neConstraint ne fc) && neConstraint.description == "ne" && neConstraint.result == orVariable
        ) {
          problem.removeConstraint(fc)
          problem.removeConstraint(neConstraint)
          problem.removeConstraint(orConstraint)
          problem.removeVariable(orVariable)

          val (x1, y1) = fc.arguments.splitAt(fc.arguments.size / 2)
          val (x2, y2) = neConstraint.arguments.splitAt(neConstraint.arguments.size / 2)

          val orScope = orConstraint.scope.filter(_ ne orVariable)

          if (orScope.size > 1) {
            problem.addConstraint(new FunctionalConstraint(fc.result, "nevec", ((x1 ++ x2) ++ (y1 ++ y2)): _*))
            problem.addConstraint(new GeneralConstraint("or", orScope: _*))
          } else {
            problem.addConstraint(new GeneralConstraint("nevec", ((x1 ++ x2) ++ (y1 ++ y2)): _*))
          }

          for (v <- x1 ++ y1 ++ x2 ++ y2; c <- v.constraints) {
            constraints.enqueue(c)
          }
          break
        }
      }
    }

  }

}
