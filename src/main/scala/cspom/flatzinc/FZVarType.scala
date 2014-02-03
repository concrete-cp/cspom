package cspom.flatzinc

import cspom.variable.BoolVariable
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMVariable
import cspom.variable.IntVariable

sealed trait FZVarType {
  def genVariable(ann: Set[String]): CSPOMExpression
}

object FZBoolean extends FZVarType {
  def genVariable(ann: Set[String]) = new BoolVariable(ann)
}

object FZFloat extends FZVarType {
  def genVariable(ann: Set[String]) = ???
}

final case class FZFloatInterval(lb: Double, ub: Double) extends FZVarType {
  def genVariable(ann: Set[String]) = ???
}

object FZInt extends FZVarType {
  def genVariable(ann: Set[String]) = ???
}

final case class FZIntInterval(lb: Int, ub: Int) extends FZVarType {
  def genVariable(ann: Set[String]) = IntVariable.ofInterval(lb, ub, ann)
}

final case class FZIntSeq(values: Seq[Int]) extends FZVarType {
  def genVariable(ann: Set[String]) = IntVariable.ofSeq(values, ann)
}

final case class FZArray(indices: IndexSet, typ: FZVarType) extends FZVarType {
  def genVariable(ann: Set[String]) = new CSPOMSeq(
    indices.toRange.map(i => typ.genVariable(Set("array_variable"))),
    indices.toRange,
    ann)

}

sealed trait IndexSet {
  def toRange: Range
}

case class FZRange(to: Int) extends IndexSet {
  def toRange = 1 to to
}
case object SomeIndexSet extends IndexSet {
  def toRange = throw new UnsupportedOperationException
}