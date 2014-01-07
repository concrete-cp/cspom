package cspom.flatzinc

import cspom.compiler.ConstraintCompiler
import cspom.CSPOMConstraint
import cspom.CSPOM
import cspom.compiler.ConstraintCompilerNoData
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMTrue
import cspom.variable.BoolVariable
import cspom.variable.BoolExpression
import cspom.variable.IntExpression

object FZPatterns {
  def apply() = Seq(
    ArrayBool,
    IntEqReif,
    new Renamer('int_ne, 'ne))
}

class Renamer(from: Symbol, to: Symbol) extends ConstraintCompilerNoData {

  def matchBool(constraint: CSPOMConstraint, problem: CSPOM) =
    constraint.function == from

  def compile(constraint: CSPOMConstraint, problem: CSPOM) = {
    replaceCtr(constraint,
      new CSPOMConstraint(constraint.result, to, constraint.arguments, constraint.params),
      problem)
  }
}

object ArrayBool extends ConstraintCompiler {

  type A = (Seq[BoolExpression], BoolExpression, Symbol)

  def mtch(constraint: CSPOMConstraint, problem: CSPOM): Option[A] = constraint match {
    case CSPOMConstraint(CSPOMTrue, 'array_bool_and,
      Seq(CSPOMSeq(_, args: Seq[BoolExpression], _, _), r: BoolExpression), _) => Some((args, r, 'and))
    case CSPOMConstraint(CSPOMTrue, 'array_bool_or,
      Seq(CSPOMSeq(_, args: Seq[BoolExpression], _, _), r: BoolExpression), _) => Some((args, r, 'or))
    case _ => None
  }

  def compile(constraint: CSPOMConstraint, problem: CSPOM, matchData: A) = {
    val (args, r, symbol) = matchData
    replaceCtr(constraint,
      new CSPOMConstraint(r, symbol, args, constraint.params),
      problem)
  }
}

object IntEqReif extends ConstraintCompiler {
  type A = (IntExpression, IntExpression, BoolExpression)
  def mtch(constraint: CSPOMConstraint, problem: CSPOM) =
    constraint match {
      case CSPOMConstraint(CSPOMTrue, 'int_eq_reif, Seq(a: IntExpression, b: IntExpression, r: BoolExpression), _) =>
        Some((a, b, r))
      case _ => None
    }

  def compile(constraint: CSPOMConstraint, problem: CSPOM, data: A) = {
    val (a, b, r) = data
    replaceCtr(constraint,
      new CSPOMConstraint(r, 'eq, Seq(a, b), constraint.params),
      problem)
  }
}