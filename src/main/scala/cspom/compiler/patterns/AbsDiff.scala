package cspom.compiler.patterns

import _root_.cspom.constraint.{ CSPOMConstraint, FunctionalConstraint }
import _root_.cspom.variable.CSPOMVariable
import _root_.cspom.CSPOM
import cspom.compiler.ProblemCompiler

/**
 * If constraint is the sub() constraint, converts a=sub(y,z), x=abs(a) to
 * x=absdiff(y,z). No other constraint may imply the auxiliary constraint a.
 */
class AbsDiff(val problem: CSPOM) extends ConstraintCompiler {

  override def compileFunctional(c: FunctionalConstraint) = {
    "sub" == c.description && c.result.auxiliary && c.result.constraints.size == 2 && {
      val process = c.result.functionalConstraints.filter {
        c => c.description == "abs" && c.arguments.sameElements(List(c.result))
      }

      if (process.hasNext) {
        for (fc <- process) {
          problem.removeConstraint(c);
          problem.removeConstraint(fc);
          problem.addConstraint(new FunctionalConstraint(
            fc.result,
            "absdiff",
            c.arguments: _*));
          problem.removeVariable(c.result)
        }
        true
      } else false
    }

  }
}