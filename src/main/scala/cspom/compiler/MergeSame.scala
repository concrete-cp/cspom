package cspom.compiler
import cspom.CSPOM
import cspom.CSPOMConstraint

/**
 * Find and merge auxiliary variables similary defined by other constraints
 */
object MergeSame extends ConstraintCompiler {

  type A = Seq[CSPOMConstraint[_]]

  override def mtch(c: CSPOMConstraint[_], problem: CSPOM): Option[A] = {
    val s = for {
      arg <- c.arguments
      if (!arg.isConstant)
      argCons <- problem.constraints(arg)
      if ((argCons ne c) &&
        c.function == argCons.function &&
        isSame(c.arguments, argCons.arguments) &&
        c.params == argCons.params)
    } yield argCons

    if (s.nonEmpty) {
      Some(s)
    } else {
      None
    }
  }

  private def isSame[A <: AnyRef](a: Seq[A], b: Seq[A]): Boolean = {
    val ai = a.iterator
    val bi = b.iterator

    while (ai.hasNext && bi.hasNext) {
      if (ai.next() ne bi.next()) return false
    }

    ai.isEmpty && bi.isEmpty

  }

  def compile(c: CSPOMConstraint[_], problem: CSPOM, same: A) = {

    val eqC = CSPOMConstraint('eq)(c.result +: same.map(_.result): _*)
    replaceCtr(same, eqC, problem)

  }

  def selfPropagation = false

}
