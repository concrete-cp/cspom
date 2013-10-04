package cspom.compiler

import cspom.CSPOMConstraint
import cspom.variable.CSPOMTrue
import cspom.CSPOM
import cspom.variable.CSPOMVariable
import cspom.variable.CSPOMExpression

trait ConstraintCompiler {
  type A
  def mtch(constraint: CSPOMConstraint, problem: CSPOM): Option[A]

  def compile(constraint: CSPOMConstraint, problem: CSPOM, matchData: A): Delta

  def replaceVars(which: Seq[CSPOMVariable], by: CSPOMExpression, in: CSPOM): Delta = {
    //println(s"Replacing $which with $by")
    val oldConstraints = which.flatMap(in.constraints).distinct
    for (c <- oldConstraints) {
      in.removeConstraint(c)
    }
    for (v <- which) {
      in.removeVariable(v)
    }
    by.flattenVariables.distinct.foreach(in.addVariable)

    val newConstraints = oldConstraints.map { c =>
      val newC = which.foldLeft(c) { (c, v) => c.replacedVar(v, by) }
      //println(s"$c to $newC")
      in.addConstraint(newC);
    }

    Delta(oldConstraints, newConstraints.flatMap(_.scope).toSet)
  }

  def replaceCtr(which: CSPOMConstraint, by: CSPOMConstraint, in: CSPOM): Delta = {
    in.removeConstraint(which)
    in.addConstraint(by)
    new Delta(Seq(which), which.scope ++ by.scope)
  }
}

trait ConstraintCompilerNoData extends ConstraintCompiler {
  type A = Unit
  def matchBool(constraint: CSPOMConstraint, problem: CSPOM): Boolean
  def mtch(constraint: CSPOMConstraint, problem: CSPOM): Option[A] =
    if (matchBool(constraint: CSPOMConstraint, problem: CSPOM)) Some(()) else None
  def compile(constraint: CSPOMConstraint, problem: CSPOM): Delta
  def compile(constraint: CSPOMConstraint, problem: CSPOM, matchData: Unit) = compile(constraint, problem: CSPOM)
}

case class Delta(removed: Seq[CSPOMConstraint], altered: Set[CSPOMVariable]) {
  /**
   * One constraint removed, and several variables altered
   */
  def this(r: CSPOMConstraint, altered: Set[CSPOMVariable]) = this(Seq(r), altered)

  /**
   *   Simply one constraint removed *
   */
  def this(r: CSPOMConstraint) = this(r, r.scope)

  def ++(d: Delta) = Delta(removed ++ d.removed, altered ++ d.altered)
  def nonEmpty = removed.nonEmpty || altered.nonEmpty
}

object Delta {
  val empty = Delta(Seq(), Set())
  def apply(): Delta = empty
}

