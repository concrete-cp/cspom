package cspom.flatzinc

import cspom.util.IntInterval
import cspom.variable.BoolVariable
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMSeq
import cspom.variable.IntVariable

sealed trait FZVarType[T] {
  def genVariable: CSPOMExpression[T]
}

object FZBoolean extends FZVarType[Boolean] {
  def genVariable = new BoolVariable()
}

case object FZFloat extends FZVarType[Double] {
  def genVariable = ???
}

final case class FZFloatInterval(lb: Double, ub: Double) extends FZVarType[Double] {
  def genVariable = ???
}

case object FZInt extends FZVarType[Int] {
  def genVariable = IntVariable.free()
}

final case class FZIntInterval(lb: Int, ub: Int) extends FZVarType[Int] {
  def genVariable = {
    if (lb == ub) {
      CSPOMConstant(lb)
    } else {
      IntVariable(IntInterval(lb, ub))
    }
  }
}

final case class FZIntSeq(values: Seq[Int]) extends FZVarType[Int] {
  def genVariable = IntVariable.ofSeq(values)
}

case object FZIntSet extends FZVarType[Int] {
  def genVariable = ???
}

final case class FZArray[T](indices: Seq[IndexSet], typ: FZVarType[T]) extends FZVarType[T] {
  def genVariable = new CSPOMSeq(
    indices.head.range.map(i => generate(indices.tail)),
    indices.head.range)

  private def generate(indices: Seq[IndexSet]): CSPOMExpression[T] = {
    if (indices.isEmpty) {
      typ.genVariable
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
