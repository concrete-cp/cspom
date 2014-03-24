package cspom.flatzinc

sealed trait SolveMode

case object Satisfy extends SolveMode

case class Minimize(e: FZExpr[_]) extends SolveMode

case class Maximize(e: FZExpr[_]) extends SolveMode

case class FZSolve(mode: SolveMode, ann: Seq[FZAnnotation])