package cspom.flatzinc

import cspom.util.IntInterval
import cspom.variable.BoolVariable
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMSeq
import cspom.variable.IntVariable
import scala.reflect.runtime.universe._
import cspom.variable.IntExpression
sealed trait FZVarType[+T] {
  implicit def tpe[B >: T]: TypeTag[B] = typeTag[B]
  def genVariable(): CSPOMExpression[T]
}

object FZBoolean extends FZVarType[Boolean] {
  def genVariable() = new BoolVariable()
}

case object FZFloat extends FZVarType[Double] {
  def genVariable() = ???
}

final case class FZFloatInterval(lb: Double, ub: Double) extends FZVarType[Double] {
  def genVariable() = ???
}

case object FZInt extends FZVarType[Int] {
  def genVariable() = IntVariable.free()
}

final case class FZIntInterval(lb: Int, ub: Int) extends FZVarType[Int] {
  def genVariable() = IntExpression(IntInterval(lb, ub))
}

final case class FZIntSeq(values: Seq[Int]) extends FZVarType[Int] {
  def genVariable() = IntExpression.ofSeq(values)
}

case object FZIntSet extends FZVarType[Int] {
  def genVariable() = ???
}

final case class FZArray[T: TypeTag](indices: Seq[Option[Range]], typ: FZVarType[T]) extends FZVarType[T] {
  def genVariable() = indices match {
    case Seq() => typ.genVariable()
    case head +: tail => new CSPOMSeq(IndexedSeq.fill(head.get.size)(FZArray(tail, typ).genVariable()), head.get)
  }
}

case class FZPredicate(annId: String, params: Seq[Any])