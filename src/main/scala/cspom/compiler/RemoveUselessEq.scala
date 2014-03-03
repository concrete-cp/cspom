package cspom.compiler

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMFalse
import cspom.variable.CSPOMTrue
import cspom.variable.CSPOMVariable
import cspom.variable.BoolVariable
import cspom.variable.CSPOMExpression

object RemoveUselessEq extends ConstraintCompiler {

  type A = CSPOMExpression[Boolean]

  override def constraintMatcher = {
    case CSPOMConstraint(res: A, 'eq, args, _) if allEqual(args) =>
      res
    case CSPOMConstraint(res: A, 'eq, args, _) if args.forall(_.isInstanceOf[CSPOMConstant[_]]) =>
      res
  }

  def compile(c: CSPOMConstraint[_], problem: CSPOM, res: A): Delta = {
    problem.removeConstraint(c)
    val delta = Delta().removed(c)

    val result = if (allEqual(c.arguments)) {
      CSPOMTrue
    } else {
      CSPOMFalse
    }

    res match {
      case b: BoolVariable => delta ++ replace(Seq(b), result, problem)
      case c: CSPOMConstant[Boolean] =>
        require(c == result); delta
      case _ => throw new IllegalArgumentException
    }

  }

  private def allEqual[A](s: Seq[A]) = s.forall(_ == s.head)

  def selfPropagation = false
}
