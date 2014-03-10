package cspom.flatzinc

import cspom.variable.BoolVariable
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMVariable
import cspom.variable.IntVariable

sealed trait FZVarType[T] {
  def genVariable(ann: Seq[FZAnnotation]): CSPOMExpression[T]
}

object FZBoolean extends FZVarType[Boolean] {
  def genVariable(ann: Seq[FZAnnotation]) = new BoolVariable(Map("fzAnnotations" -> ann))
}

case object FZFloat extends FZVarType[Double] {
  def genVariable(ann: Seq[FZAnnotation]) = ???
}

final case class FZFloatInterval(lb: Double, ub: Double) extends FZVarType[Double] {
  def genVariable(ann: Seq[FZAnnotation]) = ???
}

case object FZInt extends FZVarType[Int] {
  def genVariable(ann: Seq[FZAnnotation]) = IntVariable.free(Map("fzAnnotations" -> ann))
}

final case class FZIntInterval(lb: Int, ub: Int) extends FZVarType[Int] {
  def genVariable(ann: Seq[FZAnnotation]) = IntVariable(lb to ub, Map("fzAnnotations" -> ann))
}

final case class FZIntSeq(values: Seq[Int]) extends FZVarType[Int] {
  def genVariable(ann: Seq[FZAnnotation]) = IntVariable(values, Map("fzAnnotations" -> ann))
}

case object FZIntSet extends FZVarType[Int] {
  def genVariable(ann: Seq[FZAnnotation]) = ???
}

final case class FZArray[T](indices: Seq[IndexSet], typ: FZVarType[T]) extends FZVarType[T] {
  def genVariable(ann: Seq[FZAnnotation]) = new CSPOMSeq(
    indices.head.toRange.map(i => generate(indices.tail)),
    indices.head.toRange,
    Map("fzAnnotations" -> ann))

  private def generate(indices: Seq[IndexSet]): CSPOMExpression[T] = {
    if (indices.isEmpty) {
      typ.genVariable(Seq(FZAnnotation("array_variable")))
    } else {
      new CSPOMSeq(
        indices.head.toRange.map(i => generate(indices.tail)),
        indices.head.toRange)
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
