package cspom.flatzinc

import cspom.variable.BoolVariable
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMVariable
import cspom.variable.IntVariable
import FlatZincParser.fzAnnotations
import cspom.util.IntInterval

sealed trait FZVarType[T] {
  def genVariable(ann: Seq[FZAnnotation]): CSPOMExpression[T]
}

object FZBoolean extends FZVarType[Boolean] {
  def genVariable(ann: Seq[FZAnnotation]) = new BoolVariable(fzAnnotations(ann))
}

case object FZFloat extends FZVarType[Double] {
  def genVariable(ann: Seq[FZAnnotation]) = ???
}

final case class FZFloatInterval(lb: Double, ub: Double) extends FZVarType[Double] {
  def genVariable(ann: Seq[FZAnnotation]) = ???
}

case object FZInt extends FZVarType[Int] {
  def genVariable(ann: Seq[FZAnnotation]) = IntVariable.free(fzAnnotations(ann))
}

final case class FZIntInterval(lb: Int, ub: Int) extends FZVarType[Int] {
  def genVariable(ann: Seq[FZAnnotation]) =
    IntVariable(IntInterval(lb, ub), fzAnnotations(ann))
}

final case class FZIntSeq(values: Seq[Int]) extends FZVarType[Int] {
  def genVariable(ann: Seq[FZAnnotation]) = IntVariable.ofSeq(values, fzAnnotations(ann))
}

case object FZIntSet extends FZVarType[Int] {
  def genVariable(ann: Seq[FZAnnotation]) = ???
}

final case class FZArray[T](indices: Seq[IndexSet], typ: FZVarType[T]) extends FZVarType[T] {
  def genVariable(ann: Seq[FZAnnotation]) = new CSPOMSeq(
    indices.head.range.map(i => generate(indices.tail)),
    indices.head.range,
    fzAnnotations(ann))

  private def generate(indices: Seq[IndexSet]): CSPOMExpression[T] = {
    if (indices.isEmpty) {
      typ.genVariable(Seq())
    } else {
      new CSPOMSeq(
        indices.head.range.map(i => generate(indices.tail)),
        indices.head.range)
    }
  }

}

sealed trait IndexSet {
  def range: Range
}

case class FZRange(range: Range) extends IndexSet

case object SomeIndexSet extends IndexSet {
  def range = throw new UnsupportedOperationException
}
