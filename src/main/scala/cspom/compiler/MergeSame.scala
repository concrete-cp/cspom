package cspom.compiler
import cspom.CSPOM
import cspom.CSPOMConstraint

/**
 * Find and merge auxiliary variables similary defined by other constraints
 */
object MergeSame extends ConstraintCompiler {

  type A = Seq[CSPOMConstraint[_]]

  def functions: CompiledFunctions = AnyFunction

  override def mtch(c: CSPOMConstraint[_], problem: CSPOM): Option[A] = {
    val s = for {
      arg <- c.arguments
      if !arg.isConstant
      argCons <- problem.constraints(arg)
      if (argCons ne c) && c.result != argCons.result &&
        c.function == argCons.function &&
        isSame(c.arguments, argCons.arguments) &&
        c.params == argCons.params
    } yield argCons

    if (s.nonEmpty) {
      Some(s)
    } else {
      None
    }
  }

  private def isSame[B <: AnyRef](a: Seq[B], b: Seq[B]): Boolean = {
    val ai = a.iterator
    val bi = b.iterator

    while (ai.hasNext && bi.hasNext) {
      if (ai.next() ne bi.next()) return false
    }

    ai.isEmpty && bi.isEmpty

  }

  def compile(c: CSPOMConstraint[_], problem: CSPOM, same: A): Delta = {

    val eqC = CSPOMConstraint("eq")(c.result +: same.map(_.result): _*)
    ConstraintCompiler.replaceCtr(same.distinct, eqC, problem)

  }

  def selfPropagation = false

}
