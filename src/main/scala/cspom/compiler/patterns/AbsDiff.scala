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

        subConstraint.result.constraints.iterator
          .filter { c => c.description == "abs" && c.isInstanceOf[FunctionalConstraint] }
          .map { _.asInstanceOf[FunctionalConstraint] }
          .find { _.arguments == List(subConstraint.result) } match {
            case Some(fc) =>
              problem.removeConstraint(subConstraint);
              problem.removeConstraint(fc);
              problem.addConstraint(new FunctionalConstraint(
                result = fc.result,
                function = "absdiff",
                arguments = subConstraint.arguments));
            case None =>
          }
      }
    }
  }
}