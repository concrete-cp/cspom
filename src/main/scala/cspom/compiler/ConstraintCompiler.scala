package cspom.compiler

import cspom.CSPOMConstraint
import cspom.variable.CSPOMTrue
import cspom.CSPOM
import cspom.variable.CSPOMVariable
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMSeq

trait ConstraintCompiler {
  type A

  def mtch(c: CSPOMConstraint, p: CSPOM): Option[A] = matcher.lift((c, p)) orElse matchConstraint(c)

  def matcher: PartialFunction[(CSPOMConstraint, CSPOM), A] = PartialFunction.empty

  def matchConstraint(c: CSPOMConstraint) = constraintMatcher.lift(c)

  def constraintMatcher: PartialFunction[CSPOMConstraint, A] = PartialFunction.empty

  def compile(constraint: CSPOMConstraint, problem: CSPOM, matchData: A): Delta

  def replace(which: Seq[CSPOMExpression], by: CSPOMExpression, in: CSPOM): Delta = {
    //println(s"Replacing $which with $by")

    which.collect(in.expressionNames) match {
      case Seq() =>
      case Seq(name) => in.replaceExpression(name, by)
      case _ => throw new UnsupportedOperationException("Sorry, cannot replace multiple named expressions by one")
    }

    val oldConstraints = which.flatMap(in.constraints).distinct
    for (c <- oldConstraints) {
      in.removeConstraint(c)
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

  def replaceCtr(which: Seq[CSPOMConstraint], by: CSPOMConstraint, in: CSPOM): Delta = {
    val d = which.foldLeft(Delta()) {
      case (delta, constraint) =>
        in.removeConstraint(constraint)
        delta.removed(constraint)
    }
    in.ctr(by)
    d.added(by)
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

final case class Delta private (removed: Seq[CSPOMConstraint], altered: Set[CSPOMExpression]) {
  def removed(c: CSPOMConstraint): Delta = this ++ new Delta(Seq(c), c.fullScope.toSet)
  def removed(c: Traversable[CSPOMConstraint]): Delta =
    this ++ new Delta(c.toSeq, c.flatMap(_.fullScope).toSet)

  def added(c: CSPOMConstraint): Delta = this ++ new Delta(Nil, c.fullScope.toSet)
  def added(c: Traversable[CSPOMConstraint]): Delta =
    this ++ new Delta(Nil, c.flatMap(_.fullScope).toSet)

  def ++(d: Delta): Delta = Delta(removed ++ d.removed, altered ++ d.altered)
  def nonEmpty = removed.nonEmpty || altered.nonEmpty
}

object Delta {
  val empty = Delta(Seq(), Set())
  def apply(): Delta = empty
}

/**
 * Facilities to write easy compilers easily
 */
final class GlobalCompiler(
  override val constraintMatcher: PartialFunction[CSPOMConstraint, CSPOMConstraint])
  extends ConstraintCompiler {
  type A = CSPOMConstraint

  def compile(c: CSPOMConstraint, problem: CSPOM, data: A) = {
    replaceCtr(c, data, problem)
  }
}

object Ctr {
  def unapply(c: CSPOMConstraint): Option[(Symbol, Seq[CSPOMExpression], Map[String, Any])] = {
    if (c.result == CSPOMTrue) {
      Some((c.function, c.arguments, c.params))
    } else {
      None
    }
  }
}

object CSeq {
  def unapply[A <: CSPOMExpression](s: CSPOMSeq[A]): Option[Seq[A]] = Some(s.values)
}