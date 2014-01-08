package cspom.compiler

import cspom.CSPOMConstraint
import cspom.variable.CSPOMTrue
import cspom.CSPOM
import cspom.variable.CSPOMVariable
import cspom.variable.CSPOMExpression

trait ConstraintCompiler {
  type A

  def mtch: PartialFunction[(CSPOMConstraint, CSPOM), A] //(constraint: CSPOMConstraint, problem: CSPOM): Option[A]

  def compile(constraint: CSPOMConstraint, problem: CSPOM, matchData: A): Delta

  val constraintMatch: PartialFunction[(CSPOMConstraint, CSPOM), CSPOMConstraint] = {
    case (const, _) => const
  }

  def replaceVars(which: Seq[CSPOMVariable], by: CSPOMExpression, in: CSPOM): Delta = {
    //println(s"Replacing $which with $by")
    val oldConstraints = which.flatMap(in.constraints).distinct
    for (c <- oldConstraints) {
      in.removeConstraint(c)
    }
    for (v <- which) {
      in.removeVariable(v)
    }

    val newConstraints = oldConstraints.map { c =>
      val newC = which.foldLeft(c) { (c, v) => c.replacedVar(v, by) }
      //println(s"$c to $newC")
      in.ctr(newC);
    }

    Delta().removed(oldConstraints).added(newConstraints)
  }

  def replaceCtr(which: CSPOMConstraint, by: CSPOMConstraint, in: CSPOM): Delta = {
    in.removeConstraint(which)
    in.ctr(by)
    Delta().removed(which).added(by)
  }
}

trait ConstraintCompilerNoData extends ConstraintCompiler {
  type A = Unit
  def matchBool(constraint: CSPOMConstraint, problem: CSPOM): Boolean

  def mtch = {
    case (constraint, problem) if matchBool(constraint, problem) => ()
  }

  def compile(constraint: CSPOMConstraint, problem: CSPOM): Delta
  def compile(constraint: CSPOMConstraint, problem: CSPOM, matchData: Unit) = compile(constraint, problem: CSPOM)
}

case class Delta private (removed: Seq[CSPOMConstraint], altered: Set[CSPOMVariable]) {
  //  /**
  //   * One constraint removed, and several variables altered
  //   */
  //  def this(r: CSPOMConstraint, altered: Set[CSPOMVariable]) = this(Seq(r), altered)
  //
  //  /**
  //   *   Simply one constraint removed *
  //   */
  //  def this(r: CSPOMConstraint) = this(r, r.scope)

  def removed(c: CSPOMConstraint): Delta = this ++ new Delta(Seq(c), c.scope)
  def removed(c: Traversable[CSPOMConstraint]): Delta =
    this ++ new Delta(c.toSeq, c.flatMap(_.scope).toSet)

  def added(c: CSPOMConstraint): Delta = this ++ new Delta(Nil, c.scope)
  def added(c: Traversable[CSPOMConstraint]): Delta =
    this ++ new Delta(Nil, c.flatMap(_.scope).toSet)

  def ++(d: Delta): Delta = Delta(removed ++ d.removed, altered ++ d.altered)
  def nonEmpty = removed.nonEmpty || altered.nonEmpty
}

object Delta {
  val empty = Delta(Seq(), Set())
  def apply(): Delta = empty
}

