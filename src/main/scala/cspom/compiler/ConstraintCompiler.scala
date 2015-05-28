package cspom.compiler

import cspom.CSPOMConstraint
import cspom.variable.CSPOMConstant
import cspom.CSPOM
import cspom.variable.CSPOMVariable
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMSeq
import cspom.flatzinc.FZAnnotation
import cspom.flatzinc.FZVarParId
import com.typesafe.scalalogging.LazyLogging
import cspom.util.ContiguousIntRangeSet
import cspom.variable.SimpleExpression
import cspom.variable.IntVariable
import cspom.util.RangeSet
import cspom.util.Infinitable
import cspom.variable.IntExpression
import cspom.util.Interval
import scala.reflect.runtime.universe._

trait ConstraintCompiler extends LazyLogging {
  type A

  def mtch(c: CSPOMConstraint[_], p: CSPOM): Option[A] = matcher.lift((c, p)) orElse matchConstraint(c)

  def matcher: PartialFunction[(CSPOMConstraint[_], CSPOM), A] = PartialFunction.empty

  def matchConstraint(c: CSPOMConstraint[_]) = constraintMatcher.lift(c)

  def constraintMatcher: PartialFunction[CSPOMConstraint[_], A] = PartialFunction.empty

  def compile(constraint: CSPOMConstraint[_], problem: CSPOM, matchData: A): Delta

  def replace[T: TypeTag, S <: T](wh: CSPOMExpression[T], by: CSPOMExpression[S], in: CSPOM): Delta = {
    //println(s"Replacing $wh with $by")

    if (wh == by) {
      Delta()
    } else {

      /**
       * Constants may not be equal when replacing boolean with 0/1 variable
       */
      //      (wh, by) match {
      //        case (c1: CSPOMConstant[_], c2: CSPOMConstant[_]) =>
      //          require(c1.value == c2.value, s"Constants $c1 and $c2 differ")
      //
      //        case _ =>
      //      }

      var delta = Delta.empty
      for ((w, b) <- in.replaceExpression(wh, by)) {
        logger.debug(s"replaced $w (${in.namesOf(w)}) with $b (${in.namesOf(b)})")
        for (c <- in.constraints(w)) {

          val c2 = c.replacedVar(w, b)
          logger.debug(s"rewriting $c to $c2")
          delta ++= replaceCtr(c, c2, in)
        }
        assert(!in.isReferenced(w),
          s"${w} (${in.namesOf(w)}) is still referenced: constraints = ${in.constraints(w)}, containers = ${in.getContainers(w)}")
      }

      //lazy val newConstraints = problem.deepConstraints(merged).map(_.toString).toSeq.sorted

      //    assert(
      //      oldConstraints == newConstraints,
      //      s"${oldConstraints.mkString("\n")} is not the same as ${newConstraints.mkString("\n")}")
      //      se.filter(_ ne merged).forall(problem.constraints(_).isEmpty),
      //      s"$se is still involved by constraints: ${se.map(problem.constraints)}")
      //assert(in.deepConstraints(by).nonEmpty, s"$by (${in.namesOf(by)}) is not involved by constraints")
      assert(in.namesOf(wh).isEmpty, s"$wh (${in.namesOf(by)}) still have names: ${in.namesOf(wh)}")
      assert(in.constraints(wh).isEmpty, s"$wh (${in.namesOf(by)}) is still involved by: ${in.constraints(wh).mkString("\n")}")
      assert(in.containers(wh).isEmpty, s"$wh (${in.namesOf(by)}) is still contained in: ${in.containers(wh)}")
      delta //deltas.fold(Delta.empty)(_ ++ _)
    }
  }

  def replaceCtr(which: CSPOMConstraint[_], by: CSPOMConstraint[_], in: CSPOM): Delta = {

    removeCtr(which, in) ++ addCtr(by, in)
  }

  def replaceCtr(which: Seq[CSPOMConstraint[_]], by: CSPOMConstraint[_], in: CSPOM): Delta = {
    removeCtr(which, in) ++ addCtr(by, in)
  }

  def replaceCtr(which: CSPOMConstraint[_], by: Seq[CSPOMConstraint[_]], in: CSPOM): Delta = {
    replaceCtr(Seq(which), by, in)
  }

  def replaceCtr(which: Seq[CSPOMConstraint[_]], by: Seq[CSPOMConstraint[_]], in: CSPOM): Delta = {
    removeCtr(which, in) ++ addCtr(by, in)
  }

  def removeCtr(c: Seq[CSPOMConstraint[_]], in: CSPOM): Delta = {
    logger.debug(s"Removing $c")
    c.foreach(in.removeConstraint)
    Delta.empty.removed(c)
  }

  def removeCtr(c: CSPOMConstraint[_], in: CSPOM): Delta = removeCtr(Seq(c), in)

  def addCtr(c: CSPOMConstraint[_], in: CSPOM): Delta = addCtr(Seq(c), in)
  def addCtr(c: Seq[CSPOMConstraint[_]], in: CSPOM): Delta = {

    c.foreach(in.ctr(_))
    logger.debug(s"Adding $c")
    Delta.empty.added(c)
  }

  def selfPropagation: Boolean

  def reduceDomain(v: SimpleExpression[Int], d: Interval[Infinitable]): SimpleExpression[Int] = reduceDomain(v, RangeSet(d))

  def reduceDomain(v: SimpleExpression[Int], d: RangeSet[Infinitable]): SimpleExpression[Int] = {
    val old = IntExpression.implicits.ranges(v)
    val reduced = old & d
    if (old == reduced) {
      v
    } else {
      IntExpression(reduced)
    }
  }

  def applyDomain(v: SimpleExpression[Int], reduced: RangeSet[Infinitable]): SimpleExpression[Int] = {
    val old = IntExpression.implicits.ranges(v)
    if (old == reduced) {
      v
    } else {
      IntExpression(reduced)
    }
  }

  def reduceDomain(v: SimpleExpression[Boolean], d: Boolean): SimpleExpression[Boolean] = {
    v match {
      case b: CSPOMVariable[_] => CSPOMConstant(d)
      case c @ CSPOMConstant(b) =>
        require(b == d, s"Reduced $v to $d: empty domain")
        c
    }
  }
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

case class Delta private (
    removed: Seq[CSPOMConstraint[_]],
    added: Seq[CSPOMConstraint[_]]) {
  def removed(c: CSPOMConstraint[_]): Delta = {
    Delta(c +: removed, added.filter(_ ne c))
  }

  def removed(c: Traversable[CSPOMConstraint[_]]): Delta = {
    val cset = c.toSet
    Delta(c ++: removed, added.filterNot(cset))
  }

  def added(c: CSPOMConstraint[_]): Delta = {
    require(!removed.contains(c))
    Delta(removed, c +: added)
  }

  def added(c: Traversable[CSPOMConstraint[_]]): Delta = {
    require(c.forall(!removed.contains(_)))
    Delta(removed, c ++: added)
  }

  def ++(d: Delta): Delta = removed(d.removed).added(d.added) //Delta(removed ++ d.removed, added ++ d.added)

  def nonEmpty = removed.nonEmpty || added.nonEmpty

  override def toString = s"[ -- ${removed.mkString(", ")} ++ ${added.mkString(", ")} ]"
}

object Delta {
  val empty = Delta(Seq(), Seq())
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
  def unapply(c: CSPOMConstraint[_]): Option[(scala.Symbol, Seq[CSPOMExpression[_]], Map[String, Any])] = {
    if (c.nonReified) {
      Some((c.function, c.arguments, c.params))
    } else {
      None
    }
  }
}