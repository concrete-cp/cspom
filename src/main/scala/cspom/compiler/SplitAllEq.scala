package cspom.compiler

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.variable.CSPOMConstant
import cspom.variable.BoolVariable

object SplitAllEq extends ConstraintCompilerNoData {
  def matchBool(constraint: CSPOMConstraint[_], problem: CSPOM) =
    constraint.function == 'eq && constraint.arguments.size > 2

  def compile(constraint: CSPOMConstraint[_], problem: CSPOM) = {

    val rs = (for (Seq(a, b) <- constraint.arguments.sliding(2)) yield {
      CSPOMConstraint(new BoolVariable, 'eq, Seq(a, b))
    }).toList

    val and = CSPOMConstraint(constraint.result, 'and, rs.map(_.result))

    replaceCtr(constraint, and :: rs, problem)
//
//    problem.removeConstraint(constraint)
//    val delta = Delta().removed(constraint)
//    constraint.result match {
//      case CSPOMConstant(true) =>
//        val c = for (Seq(a, b) <- constraint.arguments.sliding(2)) yield {
//          problem.ctr(CSPOMConstraint('eq, Seq(a, b)))
//        }
//        c.foldLeft(delta)(_ added _)
//      case CSPOMConstant(false) =>
//        var d = delta
//        val rs = for (Seq(a, b) <- constraint.arguments.sliding(2)) yield {
//          val r = new BoolVariable()
//          d = d.added(problem.ctr(CSPOMConstraint(r, 'ne, Seq(a, b))))
//          r
//        }
//        d.added(problem.ctr(CSPOMConstraint('or, rs.toSeq)))
//
//      case r: BoolVariable =>
//        var d = delta
//        val rs = for (Seq(a, b) <- constraint.arguments.sliding(2)) yield {
//          val r = new BoolVariable()
//          d = d.added(problem.ctr(CSPOMConstraint(r, 'eq, Seq(a, b))))
//          r
//        }
//        d.added(problem.ctr(CSPOMConstraint(r, 'and, rs.toSeq)))
//
//      case _ => throw new IllegalArgumentException()
//    }

  }

  def selfPropagation = false
}
