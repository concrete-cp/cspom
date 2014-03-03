package cspom.flatzinc

import cspom.compiler.ConstraintCompiler
import cspom.CSPOMConstraint
import cspom.CSPOM
import cspom.compiler.ConstraintCompilerNoData
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMTrue
import cspom.variable.CSPOMExpression
import cspom.variable.IntVariable
import cspom.compiler.GlobalCompiler
import cspom.compiler.Ctr
import cspom.compiler.CSeq
import cspom.extension.Table
import cspom.variable.CSPOMVariable
import cspom.variable.CSPOMConstant

object FZPatterns {
  def apply() = Seq(new GlobalCompiler(mtch) { def selfPropagation = false }, new Flattener('allDifferent))

  val mtch: PartialFunction[CSPOMConstraint[_], CSPOMConstraint[_]] = {
    /**
     *  (∀ i ∈ 1..n : as[i]) ↔ r where n is the length of as
     * array_bool_and(array [int] of var bool: as, var bool: r)
     */
    case Ctr('array_bool_and, Seq(CSeq(as), r: CSPOMExpression[Boolean]), p) =>
      CSPOMConstraint(r, 'and, as, p)

    /**
     *  b ∈ 1..n ∧ as[b] = c where n is the length of as
     * array_bool_element(var int: b, array [int] of bool: as, var bool: c)
     *
     * TODO: b as variable
     */
    case Ctr('array_bool_element, Seq(b: CSPOMConstant[Int], CSeq(as), c: CSPOMExpression[Boolean]), p) =>
      CSPOMConstraint('eq, Seq(as(b.value), c), p)

    /**
     * (∃ i ∈ 1..n : as[i]) ↔ r where n is the length of as
     * array_bool_or(array [int] of var bool: as, var bool: r)
     */
    case Ctr('array_bool_or, Seq(CSeq(as), v: CSPOMExpression[Boolean]), p) =>
      new CSPOMConstraint(v, 'or, as, p)

    /**
     * (((i ∈ 1..n : as[i]) mod 2) = 1) where n is the length of as
     * array_bool_xor(array [int] of var bool: as)
     */

    /**
     * b ∈ 1..n ∧ as[b] = c where n is the length of as
     * array_float_element(var int: b, array [int] of float: as, var float: c)
     */
    /**
     * b ∈ 1..n ∧ as[b] = c where n is the length of as
     * array_int_element(var int: b, array [int] of int: as, var int: c)
     */
    case Ctr('array_int_element, Seq(b: CSPOMExpression[Int], as: CSPOMSeq[Int], c: CSPOMExpression[Int]), p) =>
      CSPOMConstraint('extension, Seq(b, c), Map("init" -> false, "relation" ->
        new Table(as.withIndex.map {
          case (const: CSPOMConstant[Int], i) => Seq(i, const.value)
        })))
    /**
     * b ∈ 1..n ∧ as[b] = c where n is the length of as
     * array_set_element(var int: b, array [int] of set of int: as, set of int: c)
     */
    /**
     * b ∈ 1..n ∧ as[b] = c where n is the length of as
     * array_var_bool_element(var int: b, array [int] of var bool: as, var bool: c)
     */
    /**
     * b ∈ 1..n ∧ as[b] = c where n is the length of as
     * array_var_float_element(var int: b, array [int] of var float: as, var float: c)
     */
    /**
     * b ∈ 1..n ∧ as[b] = c where n is the length of as
     * array_var_int_element(var int: b, array [int] of var int: as, var int: c)
     */
    /**
     * b ∈ 1..n ∧ as[b] = c where n is the length of as
     * array_var_set_element(var int: b, array [int] of var set of int: as, var set of int: c)
     */
    /**
     * (a ↔ b = 1) ∧ (¬a ↔ b = 0)
     * bool2int(var bool: a, var int: b)
     */
    case Ctr('bool2int, Seq(a: CSPOMExpression[Boolean], b: CSPOMExpression[Int]), p) =>
      CSPOMConstraint('eq, Seq(a, b), p)
    /**
     * (a ∧ b) ↔ r
     * bool_and(var bool: a, var bool: b, var bool: r)
     */
    /**
     * (∃ i ∈ 1..nas : as[i]) ∨ (∃ i ∈ 1..nbs : ¬bs[i]) where n is the length of as
     * bool_clause(array [int] of var bool: as, array [int] of var bool: bs)
     */
    /**
     * a = b
     * bool_eq(var bool: a, var bool: b)
     */
    case Ctr('bool_eq, a: Seq[CSPOMExpression[Boolean]], p) => CSPOMConstraint('eq, a, p)
    /**
     * (a = b) ↔ r
     * bool_eq_reif(var bool: a, var bool: b, var bool: r)
     */
    case Ctr('bool_eq_reif, Seq(a: CSPOMExpression[Boolean], b: CSPOMExpression[Boolean], r: CSPOMExpression[Boolean]), p) =>
      new CSPOMConstraint(r, 'eq, Seq(a, b), p)
    /**
     * ¬a ∨ b
     * bool_le(var bool: a, var bool: b)
     */
    case Ctr('bool_le, Seq(a: CSPOMExpression[Boolean], b: CSPOMExpression[Boolean]), p) =>
      CSPOMConstraint('or, Seq(a, b), p + ("revsign" -> Seq(true, false)))
    /**
     * (¬a ∨ b) ↔ r
     * bool_le_reif(var bool: a, var bool: b, var bool: r)
     */
    case Ctr('bool_le_reif, Seq(a: CSPOMExpression[Boolean], b: CSPOMExpression[Boolean], r: CSPOMExpression[Boolean]), p) =>
      new CSPOMConstraint(r, 'or, Seq(a, b), p + ("revsign" -> Seq(true, false)))
    /**
     * i ∈ 1..n : as[i].bs[i] = c where n is the common length of as and bs
     * bool_lin_eq(array [int] of int: as, array [int] of var bool: bs, var int: c)
     */
    /**
     * i ∈ 1..n : as[i].bs[i] ≤ c where n is the common length of as and bs
     * bool_lin_le(array [int] of int: as, array [int] of var bool: bs, int: c)
     */
    /**
     * ¬a ∧ b
     * bool_lt(var bool: a, var bool: b)
     */
    /**
     * (¬a ∧ b) ↔ r
     * bool_lt_reif(var bool: a, var bool: b, var bool: r)
     */
    /**
     * ¬a = b
     * bool_not(var bool: a, var bool: b)
     */
    /**
     * (a ∨ b) ↔ r
     * bool_or(var bool: a, var bool: b, var bool: r)
     */
    /**
     * (a = b) ↔ r
     * bool_xor(var bool: a, var bool: b, var bool: r)
     */
    /**
     * |a| = b
     * float_abs(var float: a, var float: b)
     */
    /**
     * acos a = b
     * float_acos(var float: a, var float: b)
     */
    /**
     * asin a = b
     * float_asin(var float: a, var float: b)
     */
    /**
     * atan a = b
     * float_atan(var float: a, var float: b)
     */
    /**
     * cos a = b
     * float_cos(var float: a, var float: b)
     */
    /**
     * cosh a = b
     * float_cosh(var float: a, var float: b)
     */
    /**
     * exp a = b
     * float_exp(var float: a, var float: b)
     */
    /**
     * ln a = b
     * float_ln(var float: a, var float: b)
     */
    /**
     * log 10 a = b
     * float_log10(var float: a, var float: b)
     */
    /**
     * log 2 a = b
     * float_log2(var float: a, var float: b)
     */
    /**
     * √a = b
     * float_sqrt(var float: a, var float: b)
     */
    /**
     * sin a = b
     * float_sin(var float: a, var float: b)
     */
    /**
     * sinh a = b
     * float_sinh(var float: a, var float: b)
     */
    /**
     * tan a = b
     * float_tan(var float: a, var float: b)
     */
    /**
     * tanh a = b
     * float_tanh(var float: a, var float: b)
     */
    /**
     * a = b
     * float_eq(var float: a, var float: b)
     */
    /**
     * (a = b) ↔ r
     * float_eq_reif(var float: a, var float: b, var bool: r)
     */
    /**
     * a ≤ b
     * float_le(var float: a, var float: b)
     */
    /**
     * (a ≤ b) ↔ r
     * float_le_reif(var float: a, var float: b, var bool: r)
     */
    /**
     * i ∈ 1..n : as[i].bs[i] = c where n is the common length of as and bs
     * float_lin_eq(array [int] of float: as, array [int] of var float: bs, float: c)
     */
    /**
     * (i ∈ 1..n : as[i].bs[i] = c) ↔ r where n is the common length of as and bs
     * float_lin_eq_reif(array [int] of float: as, array [int] of var float: bs, float: c, var bool: r)
     */
    /**
     * i ∈ 1..n : as[i].bs[i] ≤ c where n is the common length of as and bs
     * float_lin_le(array [int] of float: as, array [int] of var float: bs, float: c)
     */
    /**
     * (i ∈ 1..n : as[i].bs[i] ≤ c) ↔ r where n is the common length of as and bs
     * float_lin_le_reif(array [int] of float: as, array [int] of var float: bs, float: c, var bool: r)
     */
    /**
     * i ∈ 1..n : as[i].bs[i] < c where n is the common length of as and bs
     * float_lin_lt(array [int] of float: as, array [int] of var float: bs, float: c)
     */
    /**
     * (i ∈ 1..n : as[i].bs[i] < c) ↔ r where n is the common length of as and bs
     * float_lin_lt_reif(array [int] of float: as, array [int] of var float: bs, float: c, var bool: r)
     */
    /**
     * i ∈ 1..n : as[i].bs[i] = c where n is the common length of as and bs
     * float_lin_ne(array [int] of float: as, array [int] of var float: bs, float: c)
     */
    /**
     * (i ∈ 1..n : as[i].bs[i] = c) ↔ r where n is the common length of as and bs
     * float_lin_ne_reif(array [int] of float: as, array [int] of var float: bs, float: c, var bool: r)
     */
    /**
     * a < b
     * float_lt(var float: a, var float: b)
     */
    /**
     * (a < b) ↔ r
     * float_lt_reif(var float: a, var float: b, var bool: r)
     */
    /**
     * max(a, b) = c
     * float_max(var float: a, var float: b, var float: c)
     */
    /**
     * min(a, b) = c
     * float_min(var float: a, var float: b, var float: c)
     */
    /**
     * a = b
     * float_ne(var float: a, var float: b)
     */
    /**
     * (a = b) ↔ r
     * float_ne_reif(var float: a, var float: b, var bool: r)
     */
    /**
     * a+b = c
     * float_plus(var float: a, var float: b, var float: c)
     */
    /**
     * |a| = b
     * int_abs(var int: a, var int: b)
     */
    /**
     * a/b = c rounding towards zero.
     * int_div(var int: a, var int: b, var int: c)
     */
    /**
     * a = b
     * int_eq(var int: a, var int: b)
     */
    case Ctr('int_eq, args, p) => CSPOMConstraint('eq, args, p)
    /**
     * (a = b) ↔ r
     * int_eq_reif(var int: a, var int: b, var bool: r)
     */
    case Ctr('int_eq_reif, Seq(a: CSPOMExpression[Int], b: CSPOMExpression[Int], r: CSPOMExpression[Boolean]), p) =>
      CSPOMConstraint(r, 'eq, Seq(a, b), p)
    /**
     * a ≤ b
     * int_le(var int: a, var int: b)
     */
    case Ctr('int_le, a, p) => CSPOMConstraint('le, a, p)
    /**
     * (a ≤ b) ↔ r
     * int_le_reif(var int: a, var int: b, var bool: r)
     */
    case Ctr('int_le_reif, Seq(a: CSPOMExpression[Int], b: CSPOMExpression[Int], r: CSPOMExpression[Boolean]), p) =>
      new CSPOMConstraint(r, 'le, Seq(a, b), p)
    /**
     * i ∈ 1..n : as[i].bs[i] = c where n is the common length of as and bs
     * int_lin_eq(array [int] of int: as, array [int] of var int: bs, int: c)
     */
    case Ctr('int_lin_eq, Seq(CSeq(as: Seq[CSPOMConstant[Int]]), bs, c: CSPOMExpression[Int]), p) =>
      CSPOMConstraint('sum, Seq(bs, c), p + ("coefficients" -> as.map(_.value), "mode" -> "eq"))

    /**
     * i ∈ 1..n : as[i].bs[i] = c) ↔ r where n is the common length of as and bs
     * int_lin_eq_reif(array [int] of int: as, array [int] of var int: bs, int: c, var bool: r)
     */
    /**
     * i ∈ 1..n : as[i].bs[i] ≤ c where n is the common length of as and bs
     * int_lin_le(array [int] of int: as, array [int] of var int: bs, int: c)
     */
    case Ctr('int_lin_le, Seq(CSeq(as: Seq[CSPOMConstant[Int]]), bs, c: CSPOMExpression[Int]), p) =>
      CSPOMConstraint('sum, Seq(bs, c), p + ("coefficients" -> as.map(_.value), "mode" -> "le"))
    /**
     * (i ∈ 1..n : as[i].bs[i] ≤ c) ↔ r where n is the common length of as and bs
     * int_lin_le_reif(array [int] of int: as, array [int] of var int: bs, int: c, var bool: r)
     */
    /**
     * i ∈ 1..n : as[i].bs[i] = c where n is the common length of as and bs
     * int_lin_ne(array [int] of int: as, array [int] of var int: bs, int: c)
     */
    /**
     * (i ∈ 1..n : as[i].bs[i] = c) ↔ r where n is the common length of as and bs
     * int_lin_ne_reif(array [int] of int: as, array [int] of var int: bs, int: c, var bool: r)
     */
    /**
     * a < b
     * int_lt(var int: a, var int: b)
     */
    case Ctr('int_lt, Seq(a, b), p) =>
      CSPOMConstraint('gt, Seq(b, a), p)
    /**
     * (a < b) ↔ r
     * int_lt_reif(var int: a, var int: b, var bool: r)
     */
    case Ctr('int_lt_reif, Seq(a: CSPOMExpression[Int], b: CSPOMExpression[Int], r: CSPOMExpression[Boolean]), p) =>
      CSPOMConstraint(r, 'lt, Seq(a, b), p)
    /**
     * max(a, b) = c
     * int_max(var int: a, var int: b, var int: c)
     */
    /**
     * min(a, b) = c
     * int_min(var int: a, var int: b, var int: c)
     */
    /**
     * a − x.b = c where x = a/b rounding towards zero.
     * int_mod(var int: a, var int: b, var int: c)
     */
    /**
     * a = b
     * int_ne(var int: a, var int: b)
     */
    case Ctr('int_ne, Seq(a, b), p) => CSPOMConstraint('ne, Seq(a, b), p)
    /**
     * (a = b) ↔ r
     * int_ne_reif(var int: a, var int: b, var bool: r)
     */
    case Ctr('int_ne_reif, Seq(a: CSPOMExpression[Int], b: CSPOMExpression[Int], r: CSPOMExpression[Boolean]), p) =>
      new CSPOMConstraint(r, 'ne, Seq(a, b), p)
    /**
     * a+b = c
     * int_plus(var int: a, var int: b, var int: c)
     */
    case Ctr('int_plus, Seq(a: CSPOMExpression[Int], b: CSPOMExpression[Int], c: CSPOMExpression[Int]), p) =>
      new CSPOMConstraint(c, 'add, Seq(a, b), p)
    /**
     * a×b = c
     * int_times(var int: a, var int: b, var int: c)
     */
    case Ctr('int_times, Seq(a: CSPOMExpression[Int], b: CSPOMExpression[Int], c: CSPOMExpression[Int]), p) =>
      new CSPOMConstraint(c, 'mul, Seq(a, b), p)
    /**
     * a = b
     * int2float(var int: a, var float: b)
     */
    /**
     * |a| = b
     * set_card(var set of int: a, var int: b)
     */
    /**
     * a−b = c
     * set_diff(var set of int: a, var set of int: b, var set of int: c)
     */
    /**
     * a = b
     * set_eq(var set of int: a, var set of int: b)
     */
    /**
     * (a = b) ↔ r
     * set_eq_reif(var set of int: a, var set of int: b, var bool: r)
     */
    /**
     * a∈b
     * set_in(var int: a, var set of int: b)
     */
    /**
     * (a ∈ b) ↔ r
     * set_in_reif(var int: a, var set of int: b, var bool: r)
     */
    /**
     * a∩b = c
     * set_intersect(var set of int: a, var set of int: b, var set of int: c)
     */
    /**
     * a ⊆ b ∨ min(a b) ∈ a
     * set_le(var set of int: a, var set of int: b)
     */
    /**
     * a ⊂ b ∨ min(a b) ∈ a
     * set_lt(var set of int: a, var set of int: b)
     */
    /**
     * a = b
     * set_ne(var set of int: a, var set of int: b)
     */
    /**
     * (a = b) ↔ r
     * set_ne_reif(var set of int: a, var set of int: b, var bool: r)
     */
    /**
     * a⊆b
     * set_subset(var set of int: a, var set of int: b)
     */
    /**
     * (a ⊆ b) ↔ r
     * set_subset_reif(var set of int: a, var set of int: b, var bool: r)
     */
    /**
     * a b = c
     * set_symdiff(var set of int: a, var set of int: b, var set of int: c)
     */
    /**
     * a∪b = c
     * set_union(var set of int: a, var set of int: b, var set of int: c)
     */

    /**
     * FlatZinc flattens t.
     * predicate table_int(array[int] of var int: x, array[int, int] of int: t)
     */
    case Ctr('table_int, Seq(x: CSPOMSeq[Int], t: CSPOMSeq[Int]), p) =>
      CSPOMConstraint('extension, x, p ++ Map("init" -> false,
        "relation" -> new Table(t.map {
          case v: CSPOMConstant[Int] => v.value
        }.grouped(x.size).toSeq)))
  }

}

class Flattener(symbol: Symbol) extends ConstraintCompiler {
  type A = Seq[CSPOMExpression[_]]

  override def constraintMatcher = {
    case CSPOMConstraint(_, `symbol`, Seq(CSeq(args)), _) => args
  }

  def compile(constraint: CSPOMConstraint[_], problem: CSPOM, args: A) = {
    replaceCtr(constraint,
      new CSPOMConstraint(constraint.result, symbol, args, constraint.params), problem)
  }

  def selfPropagation = false
}