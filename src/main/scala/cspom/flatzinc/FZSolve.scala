package cspom.flatzinc

sealed trait SolveMode

case object Satisfy extends SolveMode

case class Minimize[T](e: FZExpr[T]) extends SolveMode

case class Maximize[T](e: FZExpr[T]) extends SolveMode

case class FZSolve(mode: SolveMode, ann: Seq[FZAnnotation])