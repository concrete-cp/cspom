package cspom.flatzinc

import cspom.variable.BoolVariable
import cspom.variable.CSPOMBool
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMType
import cspom.variable.CSPOMVariable
import cspom.variable.CSPOMDouble
import cspom.variable.CSPOMInt
import cspom.variable.CSPOMSeqType

sealed trait FZVarType {
  def genVariable(name: String, ann: Seq[String]): CSPOMExpression
  def cspomType: CSPOMType
}

object FZBoolean extends FZVarType {
  def genVariable(name: String, ann: Seq[String]) = new BoolVariable(name, ann)
  def cspomType = CSPOMBool
}

object FZFloat extends FZVarType {
  def genVariable(name: String, ann: Seq[String]) = ???
  def cspomType = CSPOMDouble
}

final case class FZFloatInterval(lb: Double, ub: Double) extends FZVarType {
  def genVariable(name: String, ann: Seq[String]) = ???
  def cspomType = CSPOMDouble
}

object FZInt extends FZVarType {
  def genVariable(name: String, ann: Seq[String]) = ???
  def cspomType = CSPOMInt
}

final case class FZIntInterval(lb: Int, ub: Int) extends FZVarType {
  def genVariable(name: String, ann: Seq[String]) = CSPOMVariable.ofInterval(name, lb, ub)
  def cspomType = CSPOMInt
}

final case class FZIntSeq(values: Seq[Int]) extends FZVarType {
  def genVariable(name: String, ann: Seq[String]) = CSPOMVariable.ofIntSeq(name, values, ann)
  def cspomType = CSPOMInt
}

final case class FZArray(indices: Range, typ: FZVarType) extends FZVarType {
  def genVariable(name: String, ann: Seq[String]) = new CSPOMSeq(
    name,
    typ.cspomType,
    indices.map(i => typ.genVariable(s"$name[$i]", Seq())),
    indices,
    ann)
  def cspomType = CSPOMSeqType(typ.cspomType)
}