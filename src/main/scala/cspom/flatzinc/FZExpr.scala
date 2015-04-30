package cspom.flatzinc

import cspom.variable.CSPOMVariable
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMConstant

sealed trait FZExpr[+A] {
  def toCSPOM(declared: Map[String, CSPOMExpression[Any]]): CSPOMExpression[_]
  //def asConstant: CSPOMExpression[_]
  def value: A
}

sealed trait FZConstant[+A] extends FZExpr[A] {
  def toCSPOM(declared: Map[String, CSPOMExpression[Any]]) = asConstant
  def asConstant: CSPOMExpression[_]
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

case class FZArrayIdx(array: String, index: Int) extends FZExpr[String] {
  def value = s"$array[$index]"

  def toCSPOM(declared: Map[String, CSPOMExpression[Any]]) =
    declared
      .get(array)
      .collect {
        case s: CSPOMSeq[_] => s(index)
      }
      .get

}

case class FZVarParId(value: String) extends FZExpr[String] {
  def toCSPOM(declared: Map[String, CSPOMExpression[Any]]) = declared(value)
  def index(i: Int) = FZArrayIdx(value, i)

}

case class FZArrayExpr[+A](value: Seq[FZExpr[A]]) extends FZExpr[Seq[FZExpr[A]]] {
  def toCSPOM(declared: Map[String, CSPOMExpression[Any]]) =
    new CSPOMSeq(
      value.map(_.toCSPOM(declared)).toIndexedSeq,
      1 to value.size)

  def asConstant(indices: Range): CSPOMSeq[_] =
    new CSPOMSeq(value
      .map {
        case c: FZConstant[_] => c.asConstant
        //case a: FZArrayExpr[_] => a.asConstant(indices)
        case _                => throw new IllegalArgumentException
      }
      .toIndexedSeq, indices)
}

case class FZStringLiteral(value: String) extends FZConstant[String] {
  def asConstant = ???
}

case class FZAnnotation(predAnnId: String, expr: Seq[FZExpr[_]] = Seq()) extends FZExpr[String] {
  def value = predAnnId + expr.mkString("(", ", ", ")")
  def toCSPOM(declared: Map[String, CSPOMExpression[Any]]) = ???
  def asConstant = ???
  def isConstant = ???
  override def toString = value

}