package cspom.compiler

import cspom.CSPOMConstraint
import cspom.variable.CSPOMTrue
import cspom.CSPOM
import cspom.variable.CSPOMVariable
import cspom.variable.CSPOMExpression

trait ConstraintCompiler {
  type A

  def mtch(c: CSPOMConstraint, p: CSPOM): Option[A] = matcher.lift((c, p)) orElse matchConstraint(c)

  def matcher: PartialFunction[(CSPOMConstraint, CSPOM), A] = PartialFunction.empty

  def matchConstraint(c: CSPOMConstraint) = constraintMatcher.lift(c)

  def constraintMatcher: PartialFunction[CSPOMConstraint, A] = PartialFunction.empty

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

    val newConstraints = for (c <- oldConstraints) yield {
      in.ctr(which.foldLeft(c) { (c, v) =>
        c.replacedVar(v, by)
      })
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

  override def mtch(constraint: CSPOMConstraint, problem: CSPOM) =
    if (matchBool(constraint, problem)) Some()
    else None

  def compile(constraint: CSPOMConstraint, problem: CSPOM): Delta
  def compile(constraint: CSPOMConstraint, problem: CSPOM, matchData: Unit) = compile(constraint, problem: CSPOM)
}

case class Delta private (removed: Seq[CSPOMConstraint], altered: Set[CSPOMVariable]) {
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

