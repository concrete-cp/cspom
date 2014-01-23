package cspom.flatzinc

import cspom.compiler.ConstraintCompiler
import cspom.CSPOMConstraint
import cspom.CSPOM
import cspom.compiler.ConstraintCompilerNoData
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMTrue
import cspom.variable.BoolExpression
import cspom.variable.IntExpression
import cspom.variable.CSPOMExpression
import cspom.variable.IntVariable
import cspom.variable.IntConstant

object FZPatterns {
  def apply() = Seq(
    ArrayBool, //array_bool_and, array_bool_or
    IntBinReif, //int_{any}_reif
    BoolLe, // bool_le 
    'int_ne ~> 'ne, // int_ne
    'int_eq ~> 'eq, // int_eq
    'all_different_int ~> 'allDifferent,
    new Flattener('allDifferent), // all_different_int
    IntLinEq)

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

class Flattener(symbol: Symbol) extends ConstraintCompiler {
  type A = Seq[CSPOMExpression]

  override def constraintMatcher = {
    case CSPOMConstraint(_, `symbol`, Seq(CSPOMSeq(_, args: Seq[CSPOMExpression], _, _)), _) => args
  }

  def compile(constraint: CSPOMConstraint, problem: CSPOM, args: A) = {
    replaceCtr(constraint,
      new CSPOMConstraint(constraint.result, symbol, args, constraint.params), problem)
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
      new CSPOMConstraint(CSPOMTrue, 'or, constraint.arguments, Map("revsign" -> Seq(true, false))), problem)
  }
}

object IntLinEq extends ConstraintCompiler {
  type A = (Seq[Int], Seq[IntVariable], IntExpression)

  override def constraintMatcher = {
    case CSPOMConstraint(CSPOMTrue, 'int_lin_eq, Seq(
      CSPOMSeq(_, factors: Seq[IntConstant], _, _),
      CSPOMSeq(_, variables: Seq[IntVariable], _, _),
      result: IntExpression), _) => (factors.map(_.value), variables, result)
  }

  override def compile(constraint: CSPOMConstraint, problem: CSPOM, data: A) = {
    val (factors, variables, result) = data
    replaceCtr(constraint,
      new CSPOMConstraint(result, 'sum, variables, constraint.params + ("coefficients" -> factors)),
      problem)
  }
}