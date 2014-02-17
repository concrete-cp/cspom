package cspom.compiler

import cspom.CSPOMConstraint
import cspom.variable.CSPOMTrue
import cspom.CSPOM
import cspom.variable.CSPOMVariable
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMSeq
import cspom.flatzinc.FZAnnotation
import cspom.flatzinc.FZVarParId

trait ConstraintCompiler {
  type A

  def mtch(c: CSPOMConstraint, p: CSPOM): Option[A] = matcher.lift((c, p)) orElse matchConstraint(c)

  def matcher: PartialFunction[(CSPOMConstraint, CSPOM), A] = PartialFunction.empty

  def matchConstraint(c: CSPOMConstraint) = constraintMatcher.lift(c)

  def constraintMatcher: PartialFunction[CSPOMConstraint, A] = PartialFunction.empty

  def compile(constraint: CSPOMConstraint, problem: CSPOM, matchData: A): Delta

  def replace(which: Seq[CSPOMExpression], by: CSPOMExpression, in: CSPOM): Delta = {
    //println(s"Replacing $which with $by")

    val names = in.namedExpressions.filter { case (name, expr) => which.contains(expr) }.keySet

    names.foreach(in.replaceExpression(_, by))
//    which.collect(in.expressionNames) match {
//      case Seq() =>
//      case Seq(name) => name.foreach(in.replaceExpression(_, by))
//      case _ => throw new UnsupportedOperationException("Sorry, cannot replace multiple named expressions by one")
//    }

    val oldConstraints = which.flatMap(in.constraints).distinct

    val newConstraints = for (c <- oldConstraints) yield {
      which.foldLeft(c) { (c, v) =>
        c.replacedVar(v, by)
      }
    }

    replaceCtr(oldConstraints, newConstraints, in)
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

  def replaceCtr(which: CSPOMConstraint, by: Seq[CSPOMConstraint], in: CSPOM): Delta = {
    replaceCtr(Seq(which), by, in)
  }

  def replaceCtr(which: Seq[CSPOMConstraint], by: Seq[CSPOMConstraint], in: CSPOM): Delta = {
    val d = which.foldLeft(Delta()) {
      case (delta, constraint) =>
        in.removeConstraint(constraint)
        delta.removed(constraint)
    }
    by.foldLeft(d) {
      case (delta, constraint) =>
        delta.added(in.ctr(constraint))
    }
  }

  def selfPropagation: Boolean
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
abstract class GlobalCompiler(
  override val constraintMatcher: PartialFunction[CSPOMConstraint, CSPOMConstraint])
  extends ConstraintCompiler {
  type A = CSPOMConstraint

  def compile(c: CSPOMConstraint, problem: CSPOM, data: A) = {
    replaceCtr(c, data, problem)
  }
}

object Ctr {
  def unapply(c: CSPOMConstraint): Option[(Symbol, Seq[CSPOMExpression], Map[String, Any])] = {
    //    val ann = c.params.get("fzAnnotations").asInstanceOf[Option[Seq[FZAnnotation]]]
    //
    //    for (as <- ann; a <- as if a.expr.contains(FZVarParId("BOOL____00100"))) {
    //      println("matching " + c)
    //      println(c.arguments.map(_.getClass()))
    //    }
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