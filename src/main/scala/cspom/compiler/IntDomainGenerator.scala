package cspom.compiler

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.util.RangeSet
import cspom.variable.CSPOMExpression
import cspom.variable.IntVariable
import cspom.variable.SimpleExpression

class IntDomainGenerator(
  val function: Symbol,
  val generators: IndexedSeq[Seq[RangeSet[Int]] => RangeSet[Int]]) extends ConstraintCompiler {

  type A = (CSPOMExpression[_], Int)

  override def mtch(c: CSPOMConstraint[_], problem: CSPOM) = {
    if (c.function == function) {
      c.fullScope.zipWithIndex.filterNot(_._1.fullyDefined) match {
        case Seq(u) => Some(u)
        case _ => None
      }
    } else {
      None
    }
  }

  def compile(c: CSPOMConstraint[_], problem: CSPOM, data: A) = {

    val allArgs = c.fullScope

    val (undefinedArg, undefinedIndex) = data

    val definedArgs = c.fullScope.filter(_ ne undefinedArg).map(
      _.asInstanceOf[IntVariable].domain)

    val generated = generators(undefinedIndex)(definedArgs)

    replace(Seq(undefinedArg), IntVariable(generated), problem)
  }

  def selfPropagation = false

}