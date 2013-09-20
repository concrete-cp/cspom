package cspom.compiler

import cspom.CSPOMConstraint
import cspom.variable.CSPOMTrue
import cspom.CSPOM
import cspom.variable.CSPOMVariable

trait ConstraintCompiler {
  type A
  def matcher(constraint: CSPOMConstraint, problem: CSPOM): Option[A]

  def compile(constraint: CSPOMConstraint, problem: CSPOM, matchData: A): Delta

  def replaceVar(which: CSPOMVariable, by: CSPOMVariable, in: CSPOM): Delta = {
    println(s"Replacing $which with $by")
    val oldConstraints = in.constraints(which)
    for (c <- oldConstraints) {
      in.removeConstraint(c)
    }
    in.removeVariable(which);
    in.addVariable(by)
    val newConstraints = for (c <- oldConstraints) yield {
      in.addConstraint(c.replacedVar(which, by));
    }
    Delta(oldConstraints, newConstraints.toList)
  }

  def replaceCtr(which: CSPOMConstraint, by: CSPOMConstraint, in: CSPOM): Delta = {
    in.removeConstraint(which)
    in.addConstraint(by)
    new Delta(which, by)
  }
}

case class Delta(removed: Seq[CSPOMConstraint], added: Seq[CSPOMConstraint]) {
  def this(r: CSPOMConstraint, a: CSPOMConstraint) = this(Seq(r), Seq(a))
  def ++(d: Delta) = Delta(removed ++ d.removed, added ++ d.removed)
  def nonEmpty = removed.nonEmpty || added.nonEmpty
}

object Delta {
  def apply(): Delta = Delta(Seq(), Seq())
}

