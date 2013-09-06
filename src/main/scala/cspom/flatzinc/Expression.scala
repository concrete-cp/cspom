package cspom.flatzinc

import cspom.variable.CSPOMVariable

sealed trait Expression {

}

case class BoolConstExpression(value: Boolean) extends Expression
case class SetConstExpression(value: Seq[Int]) extends Expression
case class FloatConstExpression(value: Double) extends Expression
case class IntConstExpression(value: Int) extends Expression
case class VariableExpression(variable: CSPOMVariable) extends Expression
case class ArrayExpression(array: Array[Expression]) extends Expression
case class AnnotationExpression(annotation: String) extends Expression
case class StringExpression(value: String) extends Expression

