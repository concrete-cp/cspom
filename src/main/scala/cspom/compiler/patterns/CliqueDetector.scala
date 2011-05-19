package cspom.compiler.patterns;

import cspom.compiler.ConstraintSelector;
import cspom.constraint.CSPOMConstraint;
import cspom.variable.CSPOMVariable;

import scala.collection.JavaConversions;

object CliqueDetector {

  def haveSubsumingConstraint(
    constraint: CSPOMConstraint, validator: CSPOMConstraint => Boolean) =
    constraint.scope.minBy(_.constraints.size).constraints.exists(
      c => c != constraint && validator(c) && subsumes(c, constraint))

  def subsumes(constraint: CSPOMConstraint, subsumed: CSPOMConstraint) =
    subsumed.scopeSet.subsetOf(constraint.scopeSet)


}
