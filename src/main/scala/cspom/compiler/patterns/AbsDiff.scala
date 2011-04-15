package cspom.compiler.patterns

import _root_.cspom.constraint.{ CSPOMConstraint, FunctionalConstraint }
import _root_.cspom.variable.CSPOMVariable
import _root_.cspom.CSPOM

/**
 * If constraint is the sub() constraint, converts a=sub(y,z), x=abs(a) to
 * x=absdiff(y,z). No other constraint may imply the auxiliary constraint a.
 */
class AbsDiff(val problem: CSPOM) extends ConstraintCompiler {

  def compile(constraint: CSPOMConstraint) {
    constraint match {
      case subConstraint: FunctionalConstraint if ("sub" == constraint.description &&
        subConstraint.result.auxiliary &&
        subConstraint.result.constraints.size == 2) => {

        for (
          c <- subConstraint.result.constraints; if (c.description == "abs")
        ) {
          c match {
            case fc: FunctionalConstraint if (fc.arguments == List(subConstraint.result)) => {

              problem.removeConstraint(subConstraint);
              problem.removeConstraint(c);
              problem.addConstraint(new FunctionalConstraint(
                result = fc.result,
                function = "absdiff",
                arguments = subConstraint.arguments));

            }
          }
        }
      }
    }
  }
}