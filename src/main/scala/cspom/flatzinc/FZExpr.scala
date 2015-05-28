package cspom.flatzinc

import cspom.variable.CSPOMVariable
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMConstant
import scala.reflect.runtime.universe._

sealed trait FZExpr[+A] {
  def toCSPOM(declared: Map[String, CSPOMExpression[Any]]): CSPOMExpression[A]
}

sealed trait FZConstant[+A] extends FZExpr[A] {
  def toCSPOM(declared: Map[String, CSPOMExpression[Any]]) = asConstant
  def value: A
  def asConstant: CSPOMExpression[A] //= CSPOMConstant(value)(tpe)
}

case class FZBoolConst(value: Boolean) extends FZConstant[Boolean] {
  def asConstant = CSPOMConstant(value)
}

case class FZSetConst(value: Seq[Int]) extends FZConstant[Seq[Int]] {
  def asConstant = CSPOMConstant(value) //??? //new CSPOMSeq[Int](value.map(CSPOMConstant(_)))
}

case class FZFloatConst(value: Double) extends FZConstant[Double] {
  def asConstant = CSPOMConstant(value)
}

case class FZIntConst(value: Int) extends FZConstant[Int] {
  def asConstant = CSPOMConstant(value)
}

case class FZArrayIdx[+A](array: String, index: Int) extends FZExpr[A] {
  def value = s"$array[$index]"

  def toCSPOM(declared: Map[String, CSPOMExpression[Any]]) =
    declared
      .get(array)
      .collect {
        case s: CSPOMSeq[A] => s(index)
      }
      .get

}

case class FZVarParId[+A](ident: String) extends FZExpr[A] {
  def toCSPOM(declared: Map[String, CSPOMExpression[Any]]) =
    declared.get(ident)
      .collect {
        case v: CSPOMExpression[A] => v
      }
      .get

  def index(i: Int) = FZArrayIdx(ident, i)

}

case class FZArrayExpr[+A: TypeTag](value: Seq[FZExpr[A]]) extends FZExpr[A] {
  def toCSPOM(declared: Map[String, CSPOMExpression[Any]]) =
    new CSPOMSeq(
      value.map(_.toCSPOM(declared)).toIndexedSeq,
      1 to value.size)

  def asConstant(indices: Range): CSPOMSeq[_] =
    new CSPOMSeq(value
      .map {
        case c: FZConstant[_]  => c.asConstant
        case a: FZArrayExpr[_] => a.asConstant(indices)
        case _                 => throw new IllegalArgumentException
      }
      .toIndexedSeq, indices)
}

case class FZStringLiteral(value: String) extends FZConstant[String] {
  def asConstant = ???
}

case class FZAnnotation(predAnnId: String, expr: Seq[FZExpr[_]] = Seq()) extends FZExpr[String] {
  def value = predAnnId + expr.mkString("(", ", ", ")")
  def toCSPOM(declared: Map[String, CSPOMExpression[Any]]) = ???
  override def toString = value

}