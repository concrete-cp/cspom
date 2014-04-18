package cspom.compiler

import cspom.CSPOMConstraint
import cspom.variable.CSPOMConstant
import cspom.CSPOM
import cspom.variable.CSPOMVariable
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMSeq
import cspom.flatzinc.FZAnnotation
import cspom.flatzinc.FZVarParId
import com.typesafe.scalalogging.slf4j.LazyLogging

trait ConstraintCompiler extends LazyLogging {
  type A

  def mtch(c: CSPOMConstraint[_], p: CSPOM): Option[A] = matcher.lift((c, p)) orElse matchConstraint(c)

  def matcher: PartialFunction[(CSPOMConstraint[_], CSPOM), A] = PartialFunction.empty

  def matchConstraint(c: CSPOMConstraint[_]) = constraintMatcher.lift(c)

  def constraintMatcher: PartialFunction[CSPOMConstraint[_], A] = PartialFunction.empty

  def compile(constraint: CSPOMConstraint[_], problem: CSPOM, matchData: A): Delta

  def replace[T, S <: T](wh: Seq[CSPOMExpression[T]], by: CSPOMExpression[S], in: CSPOM): Delta = {
    //println(s"Replacing $which with $by")

    val which = wh.filterNot(_.isInstanceOf[CSPOMConstant[_]])

    which.foreach(in.replaceExpression(_, by))

    val oldConstraints = which.flatMap(in.constraints).distinct

    val newConstraints = for (c <- oldConstraints) yield {
      which.foldLeft[CSPOMConstraint[_]](c) { (c, v) =>
        c.replacedVar(v, by)
      }
    }

    logger.debug("Replacing " + oldConstraints + " with " + newConstraints)

    replaceCtr(oldConstraints, newConstraints, in)
  }

  def replaceCtr(which: CSPOMConstraint[_], by: CSPOMConstraint[_], in: CSPOM): Delta = {
    in.removeConstraint(which)
    in.ctr(by)
    Delta().removed(which).added(by)
  }

  def replaceCtr(which: Seq[CSPOMConstraint[_]], by: CSPOMConstraint[_], in: CSPOM): Delta = {
    which.foreach(in.removeConstraint)
    val d = Delta().removed(which)
    in.ctr(by)
    d.added(by)
  }

  def replaceCtr(which: CSPOMConstraint[_], by: Seq[CSPOMConstraint[_]], in: CSPOM): Delta = {
    replaceCtr(Seq(which), by, in)
  }

  def replaceCtr(which: Seq[CSPOMConstraint[_]], by: Seq[CSPOMConstraint[_]], in: CSPOM): Delta = {
    which.foreach(in.removeConstraint)
    val dr = Delta().removed(which)

    by.foreach(in.ctr(_))

    dr.added(by)
  }

  def selfPropagation: Boolean
}

trait ConstraintCompilerNoData extends ConstraintCompiler {
  type A = Unit
  def matchBool(constraint: CSPOMConstraint[_], problem: CSPOM): Boolean

  override def mtch(constraint: CSPOMConstraint[_], problem: CSPOM) =
    if (matchBool(constraint, problem)) Some(())
    else None

  def compile(constraint: CSPOMConstraint[_], problem: CSPOM): Delta
  def compile(constraint: CSPOMConstraint[_], problem: CSPOM, matchData: Unit) = compile(constraint, problem: CSPOM)
}

final case class Delta private (removed: Seq[CSPOMConstraint[_]], altered: Set[CSPOMExpression[_]]) {
  def removed(c: CSPOMConstraint[_]): Delta = {
    new Delta(c +: removed, altered ++ c.fullScope)
  }

  def removed(c: Traversable[CSPOMConstraint[_]]): Delta = {
    new Delta(c ++: removed, altered ++ c.flatMap(_.fullScope))
  }

  def added(c: CSPOMConstraint[_]): Delta = {
    new Delta(removed, altered ++ c.fullScope)
  }

  def added(c: Traversable[CSPOMConstraint[_]]): Delta = {
    new Delta(removed, altered ++ c.flatMap(_.fullScope))
  }

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
abstract class GlobalCompiler(
  override val constraintMatcher: PartialFunction[CSPOMConstraint[_], CSPOMConstraint[_]])
  extends ConstraintCompiler {
  type A = CSPOMConstraint[_]

  def compile(c: CSPOMConstraint[_], problem: CSPOM, data: A) = {
    replaceCtr(c, data, problem)
  }
}

object Ctr {
  def unapply(c: CSPOMConstraint[_]): Option[(Symbol, Seq[CSPOMExpression[_]], Map[String, Any])] = {
    if (c.nonReified) {
      Some((c.function, c.arguments, c.params))
    } else {
      None
    }
  }
}

object CSeq {
  def unapply[A](s: CSPOMSeq[A]): Option[Seq[CSPOMExpression[_ >: A]]] =
    Some(s.values)
}