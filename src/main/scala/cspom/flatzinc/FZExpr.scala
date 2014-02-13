package cspom.flatzinc

import cspom.variable.CSPOMVariable
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMFalse
import cspom.variable.CSPOMTrue
import cspom.variable.DoubleConstant
import cspom.variable.IntConstant
import cspom.variable.CSPOMConstant

sealed trait FZExpr[A] {
  def toCSPOM(declared: Map[String, CSPOMExpression]): CSPOMExpression
  def asConstant: CSPOMExpression
  def isConstant: Boolean
  def value: A
}

sealed trait FZConstant[A] extends FZExpr[A] {
  def toCSPOM(declared: Map[String, CSPOMExpression]) = asConstant
  def isConstant = true
}

case class FZBoolConst(value: Boolean) extends FZConstant[Boolean] {
  def asConstant = if (value) CSPOMTrue else CSPOMFalse
}

case class FZSetConst(value: Seq[Int]) extends FZConstant[Seq[Int]] {
  def asConstant = ???
}

case class FZFloatConst(value: Double) extends FZConstant[Double] {
  def asConstant = DoubleConstant(value)
}

case class FZIntConst(value: Int) extends FZConstant[Int] {
  def asConstant = IntConstant(value)
}

case class FZArrayIdx(array: String, index: Int) extends FZExpr[CSPOMVariable] {
  def value = ???

  def toCSPOM(declared: Map[String, CSPOMExpression]) =
    declared.get(array).collect {
      case s: CSPOMSeq[_] => s(index)
    } get

  def isConstant = ???
  def asConstant = ???
}

case class FZVarParId(value: String) extends FZExpr[String] {
  def toCSPOM(declared: Map[String, CSPOMExpression]) = declared(value)
  def index(i: Int) = FZArrayIdx(value, i)
  def asConstant = ???
  def isConstant = false
}

case class FZArrayExpr[A](value: Seq[FZExpr[A]]) extends FZExpr[Seq[FZExpr[A]]] {
  def toCSPOM(declared: Map[String, CSPOMExpression]) =
    CSPOMSeq(value.map(_.toCSPOM(declared)): _*)

  def asConstant = CSPOMSeq(value.map(_.asConstant): _*)
  def isConstant = value.forall(_.isConstant)
}

case class FZStringLiteral(value: String) extends FZConstant[String] {
  def asConstant = ???
}

case class FZAnnotation(predAnnId: String, expr: Seq[FZExpr[_]] = Seq()) extends FZExpr[String] {
  def value = ???
  def toCSPOM(declared: Map[String, CSPOMExpression]) = ???
  def asConstant = ???
  def isConstant = ???

}