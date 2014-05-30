package cspom.compiler

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.util.RangeSet
import cspom.variable.CSPOMExpression
import cspom.variable.IntVariable
import cspom.variable.SimpleExpression
import cspom.util.Interval
import cspom.variable.CSPOMConstant

abstract class VariableCompiler(
  val function: Symbol) extends ConstraintCompiler {

  def compiler(c: CSPOMConstraint[_]): Map[CSPOMExpression[_], CSPOMExpression[_]]

  type A = Map[CSPOMExpression[_], CSPOMExpression[_]]

  implicit def ranges(e: SimpleExpression[Int]): RangeSet[Int] = e match {
    case v: IntVariable => v.domain
    case CSPOMConstant(c: Int) => RangeSet(Interval.singleton(c))
  }

  override def mtch(c: CSPOMConstraint[_], problem: CSPOM) = {
    if (c.function == function) {
      val m = compiler(c).filter { case (k, v) => k != v }
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

    //    val allArgs = c.fullScope
    //
    //    val (undefinedArg, undefinedIndex) = data
    //
    //    val definedArgs = c.fullScope.filter(_ ne undefinedArg).map {
    //      case v: IntVariable => v.domain
    //      case CSPOMConstant(c: Int) => RangeSet(GuavaRange.singleton(c))
    //    }
    //
    //    val generated = generators(c.params)(undefinedIndex)(definedArgs)
    //
    //    replace(Seq(undefinedArg), IntVariable(generated), problem)
  }

  def selfPropagation = true

}