package cspom.compiler

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.util.RangeSet
import cspom.variable.CSPOMExpression
import cspom.variable.IntVariable
import cspom.variable.SimpleExpression
import cspom.util.Interval
import cspom.variable.CSPOMConstant
import cspom.util.IntervalsArithmetic
import IntVariable.ranges
import cspom.util.IntDiscreteDomain
import com.google.common.collect.ContiguousSet
import cspom.util.ContiguousRangeSet

abstract class VariableCompiler(
  val function: Symbol) extends ConstraintCompiler {

  def compiler(c: CSPOMConstraint[_]): Map[CSPOMExpression[_], CSPOMExpression[_]]

  type A = Map[CSPOMExpression[_], CSPOMExpression[_]]

  override def mtch(c: CSPOMConstraint[_], problem: CSPOM) = {
    if (c.function == function) {
      val m = compiler(c).filter { case (k, v) => k != v }
      require(m.forall(e => c.flattenedScope.contains(e)))
      if (m.nonEmpty) {
        Some(m)
      } else {
        None
      }
    } else {
      None
    }
  }

  def compile(c: CSPOMConstraint[_], problem: CSPOM, data: A) = {
    (for ((k, v) <- data) yield {
      replace(Seq(k), v, problem)
    }).reduce(_ ++ _)
  }

  def selfPropagation = true

  def updateDomain(v: SimpleExpression[Int], d: RangeSet[Int]): SimpleExpression[Int] = {
    new ContiguousRangeSet(d, IntDiscreteDomain).singletonMatch match {
      case Some(s) => CSPOMConstant(s)
      case None => IntVariable(d, v.params)
    }

  }

  def reduceDomain(v: SimpleExpression[Int], d: RangeSet[Int]): SimpleExpression[Int] = {
    updateDomain(v, IntVariable.ranges(v) & d)
  }

}
