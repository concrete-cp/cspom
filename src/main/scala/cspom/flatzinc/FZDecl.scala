package cspom.flatzinc

import cspom.variable.CSPOMExpression

sealed trait FZDecl[A]
case class FZParamDecl[A](name: String, expression: CSPOMExpression[A]) extends FZDecl[A]
case class FZVarDecl[A](name: String, expression: Either[FZVarType[A], FZExpr[A]], annotations: Seq[FZAnnotation]) extends FZDecl[A]
