package cspom.compiler

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.util.ContiguousIntRangeSet
import cspom.util.IntRangeSet
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMVariable
import cspom.variable.IntVariable
import cspom.variable.IntVariable.ranges
import cspom.variable.SimpleExpression

abstract class VariableCompiler(
  val function: Symbol) extends ConstraintCompiler {

  def compiler(c: CSPOMConstraint[_]): Map[CSPOMExpression[_], CSPOMExpression[_]]

  type A = Map[CSPOMExpression[_], CSPOMExpression[_]]

  override def mtch(c: CSPOMConstraint[_], problem: CSPOM) = {
    if (c.function == function) {
      val m = compiler(c).filter { case (k, v) => k != v }
      require(m.forall(e => c.flattenedScope.contains(e._1)), s"$c must involve all $m")

      if (m.nonEmpty) {
        logger.info(s"$c: $m")
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

  def reduceDomain(v: SimpleExpression[Int], d: IntRangeSet): SimpleExpression[Int] = {
    val old = IntVariable.ranges(v)
    val reduced = (old & d).canonical
    if (old == reduced) {
      v
    } else {
      new ContiguousIntRangeSet(reduced).singletonMatch match {
        case Some(s) => CSPOMConstant(s, v.params)
        case None => IntVariable(reduced, v.params)
      }
    }
  }

  def applyDomain(v: SimpleExpression[Int], reduced: IntRangeSet): SimpleExpression[Int] = {
    val old = IntVariable.ranges(v)
    if (old == reduced) {
      v
    } else {
      new ContiguousIntRangeSet(reduced).singletonMatch match {
        case Some(s) => CSPOMConstant(s, v.params)
        case None => IntVariable(reduced, v.params)
      }
    }
  }

  def reduceDomain(v: SimpleExpression[Boolean], d: Boolean): SimpleExpression[Boolean] = {
    v match {
      case b: CSPOMVariable[_] => CSPOMConstant(d, b.params)
      case c @ CSPOMConstant(b) =>
        require(b == d, s"Reduced $v to $d: empty domain")
        c
    }
  }

}
