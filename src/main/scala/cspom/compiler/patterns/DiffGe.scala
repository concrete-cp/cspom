package cspom.compiler.patterns
import _root_.cspom.constraint.GeneralConstraint
import _root_.cspom.constraint.FunctionalConstraint
import _root_.cspom.constraint.CSPOMConstraint
import _root_.cspom.CSPOM

/**
 * Transforms x = sub(y, z), [t =] ge(x, k) into [t =] diffGe(y, z, k)
 */
final class DiffGe(val problem: CSPOM) extends ConstraintCompiler {

  def compile(constraint: CSPOMConstraint) {
    constraint match {
      case subConstraint: FunctionalConstraint if (subConstraint.description == "sub" &&
        subConstraint.result.auxiliary &&
        subConstraint.result.constraints.size == 2) => {

        subConstraint.result.constraints
          .iterator.filter { _.description == "ge " }
          .find { c: CSPOMConstraint =>
            val scope = c match {
              case fc: FunctionalConstraint => fc.arguments
              case ge: GeneralConstraint => ge.scope
            }
            scope.size == 2 && scope(0) == subConstraint.result
          } match {
            case Some(geConstraint) => {

              problem.removeConstraint(subConstraint);
              problem.removeConstraint(geConstraint);

              geConstraint match {
                case fc: FunctionalConstraint =>
                  problem.addConstraint(new FunctionalConstraint(
                    result = fc.result,
                    function = "diffGe",
                    arguments = subConstraint.arguments :+ fc.arguments(1)))
                case _ =>
                  problem.addConstraint(new GeneralConstraint(description = "diffGe",
                    scope = subConstraint.arguments :+ geConstraint.scope(1)))

              }
            }
            case None =>
          }
      }
    }

  }

  //
  //    private static CSPOMConstraint geConstraint(final CSPOMVariable variable) {
  //        final CSPOMConstraint geConstraint;
  //        try {
  //            geConstraint = Iterables.find(variable.getConstraints(),
  //                    CSPOMConstraint.matchesDescription("ge"));
  //        } catch (NoSuchElementException e) {
  //            return null;
  //        }
  //        final List<CSPOMVariable> scope;
  //        if (geConstraint instanceof FunctionalConstraint) {
  //            scope = ((FunctionalConstraint) geConstraint).getArguments();
  //        } else {
  //            scope = geConstraint.getScope();
  //        }
  //        if (scope.size() == 2 && scope.get(0) == variable) {
  //            return geConstraint;
  //        }
  //        return null;
  //    }

}
