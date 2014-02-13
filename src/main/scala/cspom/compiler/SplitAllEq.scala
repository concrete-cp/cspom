package cspom.compiler

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.variable.CSPOMTrue
import cspom.variable.BoolVariable

import cspom.variable.CSPOMFalse


object SplitAllEq extends ConstraintCompilerNoData {
  def matchBool(constraint: CSPOMConstraint, problem: CSPOM) =
    constraint.function == 'eq && constraint.arguments.size > 2

  def compile(constraint: CSPOMConstraint, problem: CSPOM) = {

    problem.removeConstraint(constraint)
    val delta = Delta().removed(constraint)
    constraint.result match {
      case CSPOMTrue =>
        val c = for (Seq(a, b) <- constraint.arguments.sliding(2)) yield {
          problem.ctr(new CSPOMConstraint('eq, a, b))
        }
        c.foldLeft(delta)(_ added _)
      case CSPOMFalse =>
        var d = delta
        val rs = for (Seq(a, b) <- constraint.arguments.sliding(2)) yield {
          val r = new BoolVariable()
          d = d.added(problem.ctr(new CSPOMConstraint(r, 'ne, a, b)))
          r
        }
        d.added(problem.ctr(new CSPOMConstraint('or, rs.toSeq: _*)))

      case r: BoolVariable =>
        var d = delta
        val rs = for (Seq(a, b) <- constraint.arguments.sliding(2)) yield {
          val r = new BoolVariable()
          d = d.added(problem.ctr(new CSPOMConstraint(r, 'eq, a, b)))
          r
        }
        d.added(problem.ctr(new CSPOMConstraint(r, 'and, rs.toSeq: _*)))

      case _ => throw new IllegalArgumentException()
    }

  }

  def selfPropagation = false
}
