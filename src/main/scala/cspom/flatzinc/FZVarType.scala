package cspom.flatzinc

import cspom.variable.BoolVariable
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMVariable
import cspom.variable.IntVariable

sealed trait FZVarType {
  def genVariable(ann: Seq[FZAnnotation]): CSPOMExpression
}

object FZBoolean extends FZVarType {
  def genVariable(ann: Seq[FZAnnotation]) = new BoolVariable(ann.toSet)
}

case object FZFloat extends FZVarType {
  def genVariable(ann: Seq[FZAnnotation]) = ???
}

final case class FZFloatInterval(lb: Double, ub: Double) extends FZVarType {
  def genVariable(ann: Seq[FZAnnotation]) = ???
}

case object FZInt extends FZVarType {
  def genVariable(ann: Seq[FZAnnotation]) = IntVariable.free(ann: _*)
}

final case class FZIntInterval(lb: Int, ub: Int) extends FZVarType {
  def genVariable(ann: Seq[FZAnnotation]) = IntVariable.ofInterval(lb, ub, ann.toSet)
}

final case class FZIntSeq(values: Seq[Int]) extends FZVarType {
  def genVariable(ann: Seq[FZAnnotation]) = IntVariable.ofSeq(values, ann.toSet)
}

case object FZIntSet extends FZVarType {
  def genVariable(ann: Seq[FZAnnotation]) = ???
}

final case class FZArray(indices: Seq[IndexSet], typ: FZVarType) extends FZVarType {
  def genVariable(ann: Seq[FZAnnotation]) = new CSPOMSeq(
    indices.head.toRange.map(i => generate(indices.tail)),
    indices.head.toRange,
    ann.toSet)

  private def generate(indices: Seq[IndexSet]): CSPOMExpression = {
    if (indices.isEmpty) {
      typ.genVariable(Seq(FZAnnotation("array_variable")))
    } else {
      new CSPOMSeq(
        indices.head.toRange.map(i => generate(indices.tail)),
        indices.head.toRange,
        Set(FZAnnotation("array_seq")))
    }
  }

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