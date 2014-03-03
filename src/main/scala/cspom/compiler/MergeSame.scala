package cspom.compiler
import cspom.CSPOM
import cspom.CSPOMConstraint

/**
 * Find and merge auxiliary variables similary defined by other constraints
 */
object MergeSame extends ConstraintCompiler {

  type A = CSPOMConstraint[Any]

  override def mtch(c: CSPOMConstraint[_], problem: CSPOM): Option[A] = {
    c.fullScope.iterator.flatMap(problem.constraints).filter(_ ne c).collectFirst {
      case (same @ CSPOMConstraint(_, c.function, args, c.params)) if (isSame(args, c.arguments)) =>
        same.asInstanceOf[CSPOMConstraint[Any]]
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

  def compile(c: CSPOMConstraint[_], problem: CSPOM, same: CSPOMConstraint[Any]) = {

    val eqC = CSPOMConstraint('eq, c.result, same.result)
    replaceCtr(c, eqC, problem)

  }

  def selfPropagation = false

}
