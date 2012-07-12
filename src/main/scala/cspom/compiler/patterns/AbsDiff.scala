package cspom.compiler.patterns

import _root_.cspom.constraint.{ CSPOMConstraint, FunctionalConstraint }
import _root_.cspom.variable.CSPOMVariable
import _root_.cspom.CSPOM

/**
 * If constraint is the sub() constraint, converts a=sub(y,z), x=abs(a) to
 * x=absdiff(y,z). No other constraint may imply the auxiliary constraint a.
 */
class AbsDiff(val problem: CSPOM) extends ConstraintCompiler {

  override def compileFunctional(c: FunctionalConstraint) {
    if ("sub" == c.description && c.result.auxiliary && c.result.constraints.size == 2) {

      for (
        fc <- c.result.functionalConstraints;
        if fc.description == "abs" && fc.arguments.sameElements(List(c.result))
      ) {

        problem.removeConstraint(c);
        problem.removeConstraint(fc);
        problem.addConstraint(new FunctionalConstraint(
          fc.result,
          "absdiff",
          c.arguments: _*));
        problem.removeVariable(c.result)

      }
    }
  }
}