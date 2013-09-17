package cspom.flatzinc

import cspom.variable.CSPOMExpression
import cspom.variable.BoolVariable
import cspom.variable.CSPOMVariable
import cspom.variable.CSPOMSeq

sealed trait FZVarType {
  def genVariable(name: String, ann: Seq[String]): CSPOMExpression
}

object FZBoolean extends FZVarType {
  def genVariable(name: String, ann: Seq[String]) = new BoolVariable(name, ann)
}

object FZFloat extends FZVarType {
  def genVariable(name: String, ann: Seq[String]) = ???
}

final case class FZFloatInterval(lb: Double, ub: Double) extends FZVarType {
  def genVariable(name: String, ann: Seq[String]) = ???
}

object FZInt extends FZVarType {
  def genVariable(name: String, ann: Seq[String]) = ???
}

final case class FZIntInterval(lb: Int, ub: Int) extends FZVarType {
  def genVariable(name: String, ann: Seq[String]) = CSPOMVariable.ofInterval(name, lb, ub)
}

final case class FZIntSeq(values: Seq[Int]) extends FZVarType {
  def genVariable(name: String, ann: Seq[String]) = CSPOMVariable.ofIntSeq(name, values, ann)
}

final case class FZArray(indices: Range, typ: FZVarType) extends FZVarType {
  def genVariable(name: String, ann: Seq[String]) = new CSPOMSeq(
    name,
    indices.map(i => typ.genVariable(s"$name[$i]", Seq())),
    indices,
    ann)
}