package cspom.flatzinc

import cspom.variable.CSPOMVariable
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMFalse
import cspom.variable.CSPOMTrue
import cspom.variable.DoubleConstant
import cspom.variable.IntConstant

sealed trait FZExpr[A] {
  type VarMap = Map[String, CSPOMVariable]
  type SeqMap = Map[String, CSPOMSeq[CSPOMExpression]]

  def toCSPOM(vars: VarMap, seqs: SeqMap): CSPOMExpression
  def value: A
}

case class FZBoolConst(value: Boolean) extends FZExpr[Boolean] {
  def toCSPOM(vars: VarMap, seqs: SeqMap) = {
    if (value) CSPOMTrue else CSPOMFalse
  }
}

case class FZSetConst(value: Seq[Int]) extends FZExpr[Seq[Int]] {
  def toCSPOM(vars: VarMap, seqs: SeqMap) = ???
}

case class FZFloatConst(value: Double) extends FZExpr[Double] {
  def toCSPOM(vars: VarMap, seqs: SeqMap) = DoubleConstant(value)
}

case class FZIntConst(value: Int) extends FZExpr[Int] {
  def toCSPOM(vars: VarMap, seqs: SeqMap) = IntConstant(value)
}

case class FZArrayIdx(array: String, index: Int) extends FZExpr[CSPOMVariable] {
  def value = ???
  def toCSPOM(vars: VarMap, seqs: SeqMap) = seqs(array)(index)
}

case class FZVarParId(value: String) extends FZExpr[String] {

  def toCSPOM(vars: VarMap, seqs: SeqMap) = vars.getOrElse(value, seqs(value))
  def index(i: Int) = FZArrayIdx(value, i)
}

case class FZArrayExpr[A](value: Seq[FZExpr[A]]) extends FZExpr[Seq[FZExpr[A]]] {
  def toCSPOM(vars: VarMap, seqs: SeqMap) =
    CSPOMSeq(value.map(_.toCSPOM(vars, seqs)): _*)
}

case class FZStringLiteral(value: String) extends FZExpr[String] {
  def toCSPOM(vars: VarMap, seqs: SeqMap) = ???
}

case class FZAnnotation(predAnnId: String, expr: Seq[FZExpr[_]] = Seq()) extends FZExpr[String] {
  def value = ???
  def toCSPOM(vars: VarMap, seqs: SeqMap) = ???
}