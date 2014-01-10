package cspom.flatzinc

import cspom.compiler.ConstraintCompiler
import cspom.CSPOMConstraint
import cspom.CSPOM
import cspom.compiler.ConstraintCompilerNoData
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMTrue
import cspom.variable.BoolExpression
import cspom.variable.IntExpression

object FZPatterns {
  def apply() = Seq(
    ArrayBool,
    IntBinReif,
    'int_ne ~> 'ne,
    'int_eq ~> 'eq)

  implicit class RenSymbol(s: Symbol) {
    def ~>(s2: Symbol) = new Renamer(s, s2)
  }
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

  override def constraintMatcher = {
    case CSPOMConstraint(CSPOMTrue, 'array_bool_and,
      Seq(CSPOMSeq(_, args: Seq[BoolExpression], _, _), r: BoolExpression), _) => (args, r, 'and)
    case CSPOMConstraint(CSPOMTrue, 'array_bool_or,
      Seq(CSPOMSeq(_, args: Seq[BoolExpression], _, _), r: BoolExpression), _) => (args, r, 'or)
  }

  def compile(constraint: CSPOMConstraint, problem: CSPOM, matchData: A) = {
    val (args, r, symbol) = matchData
    replaceCtr(constraint,
      new CSPOMConstraint(r, symbol, args, constraint.params),
      problem)
  }
}

object IntBinReif extends ConstraintCompiler {

  val pattern = """'int_([^_]+)_reif""".r

  type A = (IntExpression, IntExpression, BoolExpression, String)

  override def matchConstraint(constraint: CSPOMConstraint) = constraintMatcher.lift(constraint).collect {
    case (a, b, r, pattern(s)) => (a, b, r, s)
  }

  override def constraintMatcher = {
    case CSPOMConstraint(CSPOMTrue, s, Seq(a: IntExpression, b: IntExpression, r: BoolExpression), _) =>
      (a, b, r, s.toString)
  }

  def compile(constraint: CSPOMConstraint, problem: CSPOM, data: A) = {
    val (a, b, r, s) = data
    replaceCtr(constraint,
      new CSPOMConstraint(r, Symbol(s), Seq(a, b), constraint.params),
      problem)
  }
}

/**
 * Boolean <= is definitely implication.
 */
object BoolLe extends ConstraintCompilerNoData {
  def matchBool(constraint: CSPOMConstraint, problem: CSPOM) =
    constraint.function == 'bool_le && constraint.arguments.size == 2

  def compile(constraint: CSPOMConstraint, problem: CSPOM) = {
    replaceCtr(constraint,
      new CSPOMConstraint(CSPOMTrue, 'or, constraint.arguments, Map("revsign" -> Array(1, 0))), problem)
  }
}
