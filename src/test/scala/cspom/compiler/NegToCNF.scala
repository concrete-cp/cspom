package cspom.compiler

import cspom.{CSPOM, CSPOMConstraint}
import cspom.variable.CSPOMSeq

/**
  * Negation is converted to CNF :
  *
  * a = -b <=> (a v b) ^ (-a v -b)
  **/
object NegToCNF extends ConstraintCompilerNoData {

  def functions = Functions("not")

  override def matchBool(c: CSPOMConstraint[_], p: CSPOM) = true

  def compile(fc: CSPOMConstraint[_], problem: CSPOM): Delta = {

    val res = fc.result
    val Seq(arg) = fc.arguments

    val newConstraints = Seq(
      CSPOMConstraint("clause")(CSPOMSeq(res, arg), CSPOMSeq()),
      CSPOMConstraint("clause")(CSPOMSeq(), CSPOMSeq(res, arg)))

    ConstraintCompiler.replaceCtr(fc, newConstraints, problem)
  }

  def selfPropagation = false
}
