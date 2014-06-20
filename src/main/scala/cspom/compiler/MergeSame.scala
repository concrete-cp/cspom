package cspom.compiler
import cspom.CSPOM
import cspom.CSPOMConstraint

/**
 * Find and merge auxiliary variables similary defined by other constraints
 */
object MergeSame extends ConstraintCompiler {

  type A = Seq[CSPOMConstraint[_]]

  override def mtch(c: CSPOMConstraint[_], problem: CSPOM): Option[A] = {
    val s = c.arguments.flatMap(problem.constraints).filter(_ ne c).distinct.collect {
      case (same @ CSPOMConstraint(
        _, c.function, args, c.params)) if (isSame(args, c.arguments)) =>
        same
    }
    if (s.nonEmpty) {
      Some(s)
    } else {
      None
    }
  }

  @annotation.tailrec
  private def isSame[A <: AnyRef](a: Seq[A], b: Seq[A]): Boolean = {
    if (a.isEmpty) {
      b.isEmpty
    } else {
      b.nonEmpty && (a.head eq b.head) && isSame(a.tail, b.tail)
    }
  }

  def compile(c: CSPOMConstraint[_], problem: CSPOM, same: A) = {

    val eqC = CSPOMConstraint('eq, c.result +: same.map(_.result))
    replaceCtr(same, eqC, problem)

  }

  def selfPropagation = false

}
