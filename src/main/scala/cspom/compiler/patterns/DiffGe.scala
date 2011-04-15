package cspom.compiler.patterns

import _root_.cspom.constraint.FunctionalConstraint
import _root_.cspom.constraint.CSPOMConstraint
import _root_.cspom.CSPOM

final class DiffGe(val problem: CSPOM) extends ConstraintCompiler {

    def compile(constraint: CSPOMConstraint) {
      constraint match {
        case subConstraint: FunctionalConstraint 
        if (subConstraint.description == "sub" && 
            subConstraint.result.auxiliary &&
            subConstraint.result.constraints.size == 2) => {
            
              
              subConstraint.result.constraints find { _.description == "ge " }
              
        final CSPOMConstraint geConstraint = geConstraint(result);
        if (geConstraint == null) {
            return;
        }
        problem.removeConstraint(subConstraint);
        problem.removeConstraint(geConstraint);

        if (geConstraint instanceof FunctionalConstraint) {
            final CSPOMVariable[] scope = new CSPOMVariable[] {
                    subConstraint.getVariable(1), subConstraint.getVariable(2),
                    geConstraint.getVariable(2) };
            problem.addConstraint(new FunctionalConstraint(geConstraint
                    .getVariable(0), "diffGe", null, scope));
        } else {
            final CSPOMVariable[] scope = new CSPOMVariable[] {
                    subConstraint.getVariable(1), subConstraint.getVariable(2),
                    geConstraint.getVariable(1) };
            problem.addConstraint(new GeneralConstraint("diffGe", null, scope));
        }
        }
      }
     
    }
    
    def findMatch[T](it: Iterable[_<:T], clazz: Class[T]): Option[T] = {
       for (e <- it; if clazz.isInstance(e)) {
         Some(e)
       }
       None
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
