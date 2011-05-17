package cspom.compiler.patterns;

import cspom.compiler.ConstraintSelector;
import cspom.constraint.CSPOMConstraint;
import cspom.variable.CSPOMVariable;

import scala.collection.JavaConversions;


final class CliqueDetector {

  var haveEdge = Map[Set[CSPOMVariable], Boolean]()

  def allEdges(variable: CSPOMVariable, vars: Iterable[CSPOMVariable],
    validator: CSPOMConstraint => Boolean) = {
    !vars.exists(!edge(_, variable, validator))
  }

  def edge(var1: CSPOMVariable,
    var2: CSPOMVariable, validator: CSPOMConstraint => Boolean) = {
    val pair = Set(var1, var2);

    haveEdge.get(pair) match {
      case Some(edge) => edge
      case None => {

        if (var1.constraints.exists(c => validator(c) && var2.constraints.contains(c))) {
          haveEdge += pair -> true
          true
        } else {
          haveEdge += pair -> false
          false
        }

      }

    }
  }
}

object CliqueDetector {

  def haveSubsumingConstraint(
    constraint: CSPOMConstraint, validator: CSPOMConstraint => Boolean) =
    constraint.scope.head.constraints.exists(
      c => c != constraint && validator(c) && subsumes(c, constraint))

  def subsumes(constraint: CSPOMConstraint, subsumed: CSPOMConstraint) =
    !subsumed.scope.exists(!constraint.involves(_))

}
