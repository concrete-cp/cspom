package cspom.compiler

import scala.reflect.runtime.universe._
import com.typesafe.scalalogging.LazyLogging
import cspom.{CSPOM, CSPOMConstraint, UNSATException}
import cspom.util.Infinitable
import cspom.util.Interval
import cspom.util.RangeSet
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMSeq
import cspom.variable.IntExpression
import cspom.variable.SimpleExpression

object ConstraintCompiler extends LazyLogging {
  def replace[T: TypeTag, S <: T](wh: CSPOMExpression[T], by: CSPOMExpression[S], in: CSPOM): Delta = {
    //println(s"Replacing $wh with $by")
    if (by.searchSpace == 0) {
      throw new UNSATException(s"Replaced $wh with $by")
    }
    if (wh == by) {
      Delta()
    } else {
      require(!wh.isInstanceOf[CSPOMConstant[_]], s"Cannot replace $wh with $by: $wh is a constant")
      /*
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
        logger.debug(s"replaced ${w.toString(in.displayName)} with ${b.toString(in.displayName)}")

        for (c <- in.constraints(w)) {

          val c2 = c.replacedVar(w, b)
          logger.debug(s"rewriting ${c.toString(in.displayName)} to ${c2.toString(in.displayName)}")
          delta ++= replaceCtr(c, c2, in)
        }
//        assert(!in.isReferenced(w),
//          s"$w (${in.namesOf(w)}) is still referenced: constraints = ${in.constraints(w)}, " +
//            s"containers = ${in.expressionMap.getContainers(w).map(_._1.toString(in.displayName))}")
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
      assert(in.expressionMap.getContainers(wh).isEmpty,
        s"$wh (${in.namesOf(by)}) is still contained in: ${in.expressionMap.getContainers(wh)}")
      delta //deltas.fold(Delta.empty)(_ ++ _)
    }
  }

  def deepReplace(e: CSPOMExpression[_], a: CSPOMExpression[_], problem: CSPOM): Delta = {
    (e, a) match {
      case (w: CSPOMSeq[_], b: CSPOMSeq[_]) =>
        require(w.indices == b.indices)

        (w.values, b.values).zipped.foldLeft(replace(w, b, problem)) {
          case (acc, (wv, bv)) => acc ++ deepReplace(wv, bv, problem)
        }
      case (wv: SimpleExpression[_], bv: SimpleExpression[_]) =>
        replace(wv, bv, problem)
      case (e, f) => throw new IllegalArgumentException(s"Incompatible replacement $e by $f")
    }
  }

  def replaceCtr(which: CSPOMConstraint[_], by: CSPOMConstraint[_], in: CSPOM): Delta = {
    removeCtr(which, in) ++ addCtr(by, in)
  }

  def removeCtr(c: Seq[CSPOMConstraint[_]], in: CSPOM): Delta = {
    logger.debug(c.map(_.toString(in.displayName)).mkString("Removing ", ", ", ""))
    c.foreach(in.removeConstraint)
    Delta.empty.removed(c)
  }

  def removeCtr(c: CSPOMConstraint[_], in: CSPOM): Delta = removeCtr(Seq(c), in)

  def replaceCtr(which: Seq[CSPOMConstraint[_]], by: CSPOMConstraint[_], in: CSPOM): Delta = {
    removeCtr(which, in) ++ addCtr(by, in)
  }

  def addCtr(c: CSPOMConstraint[_], in: CSPOM): Delta = addCtr(Seq(c), in)
  def addCtr(c: Seq[CSPOMConstraint[_]], in: CSPOM): Delta = {
    val posted = c.flatMap(in.ctrNetwork)
    logger.debug(posted.map(_.toString(in.displayName)).mkString("Adding ", ", ", ""))
    Delta.empty.added(posted)
  }


  def replaceCtr(which: CSPOMConstraint[_], by: Seq[CSPOMConstraint[_]], in: CSPOM): Delta = {
    replaceCtr(Seq(which), by, in)
  }

  def replaceCtr(which: Seq[CSPOMConstraint[_]], by: Seq[CSPOMConstraint[_]], in: CSPOM): Delta = {
    removeCtr(which, in) ++ addCtr(by, in)
  }

  def reduceDomain[B](v: SimpleExpression[B], d: Interval[Infinitable]): SimpleExpression[B] =
    reduceDomain(v, RangeSet(d))

  def reduceDomain[B](v: SimpleExpression[B], d: RangeSet[Infinitable]): SimpleExpression[B] = {
    v.intersected(IntExpression(d))
  }

  def applyDomain(v: SimpleExpression[Int], reduced: RangeSet[Infinitable]): SimpleExpression[Int] = {
    val old = IntExpression.implicits.ranges(v)
    if (old == reduced) {
      v
    } else {
      IntExpression(reduced)
    }
  }

  def reduceDomain[B](v: SimpleExpression[B], d: Boolean): SimpleExpression[B] = {
    v.intersected(CSPOMConstant(d))
  }
}

trait Compiler {
  override def toString: String = getClass.getSimpleName
}

trait ConstraintCompiler extends Compiler with LazyLogging {
  type A

  def functions: CompiledFunctions

  def mtch(c: CSPOMConstraint[_], p: CSPOM): Option[A] = matcher.lift((c, p)) orElse matchConstraint(c)

  def matcher: PartialFunction[(CSPOMConstraint[_], CSPOM), A] = PartialFunction.empty

  def matchConstraint(c: CSPOMConstraint[_]): Option[A] = constraintMatcher.lift(c)

  def constraintMatcher: PartialFunction[CSPOMConstraint[_], A] = PartialFunction.empty

  def compile(constraint: CSPOMConstraint[_], problem: CSPOM, matchData: A): Delta
}

trait ProblemCompiler extends Compiler {
  def apply(cspom: CSPOM): Delta
}

trait ConstraintCompilerNoData extends ConstraintCompiler {
  type A = Unit
  def matchBool(constraint: CSPOMConstraint[_], problem: CSPOM): Boolean

  override def mtch(constraint: CSPOMConstraint[_], problem: CSPOM): Option[Unit] =
    if (matchBool(constraint, problem)) Some(())
    else None

  def compile(constraint: CSPOMConstraint[_], problem: CSPOM): Delta
  def compile(constraint: CSPOMConstraint[_], problem: CSPOM, matchData: Unit): Delta =
    compile(constraint, problem: CSPOM)
}

case class Delta private (
    removed: List[CSPOMConstraint[_]],
    added: List[CSPOMConstraint[_]]) {
  def removed(c: CSPOMConstraint[_]): Delta = {
    Delta(c :: removed, added.filter(_ ne c))
  }

  def removed(c: Traversable[CSPOMConstraint[_]]): Delta = {
    val cset = c.toSet
    Delta(c ++: removed, added.filterNot(cset))
  }

  def added(c: CSPOMConstraint[_]): Delta = {
    assert(!removed.contains(c))
    Delta(removed, c :: added)
  }

  def added(c: Traversable[CSPOMConstraint[_]]): Delta = {
    assert(c.forall(!removed.contains(_)))
    Delta(removed, c ++: added)
  }

  def ++(d: Delta): Delta = removed(d.removed).added(d.added) //Delta(removed ++ d.removed, added ++ d.added)

  def nonEmpty: Boolean = removed.nonEmpty || added.nonEmpty

  override def toString = s"[ -- ${removed.mkString(", ")} ++ ${added.mkString(", ")} ]"
  def toString(cspom: CSPOM) = s"[ -- \n${removed.map(e => e.toString(cspom.displayName)).mkString("\n")}\n ++ \n${added.map(_.toString(cspom.displayName)).mkString("\n")} ]"
}

object Delta {
  val empty = Delta(List(), List())
  def apply(): Delta = empty

}

/**
 * Facilities to write easy compilers easily
 */
trait GlobalCompiler extends ConstraintCompiler {
  type A = Seq[CSPOMConstraint[_]]

  def compile(c: CSPOMConstraint[_], problem: CSPOM, data: A): Delta = {
    ConstraintCompiler.replaceCtr(c, data, problem)
  }
}

object Ctr {
  def unapply(c: CSPOMConstraint[_]): Option[(String, Seq[CSPOMExpression[_]], Map[String, Any])] = {
    if (c.nonReified) {
      Some((c.function, c.arguments, c.params))
    } else {
      None
    }
  }
}