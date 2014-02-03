package cspom.variable

import cspom.CSPOM
import scala.collection.mutable.WeakHashMap

/*
 * An expression can be either simple (a variable or a constant) or a sequence of expressions
 */
sealed trait CSPOMExpression {
  def flattenVariables: Seq[CSPOMVariable]

  def replaceVar(which: CSPOMExpression, by: CSPOMExpression): CSPOMExpression

  def intersected(that: CSPOMExpression): CSPOMExpression

  def contains(that: CSPOMConstant): Boolean

  def params: Set[String]
}

/*
 * Simple expressions are typed (int or boolean)
 */
sealed trait SimpleExpression extends CSPOMExpression {
  final def replaceVar(which: CSPOMExpression, by: CSPOMExpression) =
    if (which == this) by else this
}

trait IntExpression extends SimpleExpression

trait BoolExpression extends SimpleExpression {
  def neg: BoolExpression
}

trait CSPOMConstant extends SimpleExpression {
  def flattenVariables = Seq()

  def contains(that: CSPOMConstant) = this == that

  def intersected(that: CSPOMExpression) =
    if (that.contains(this)) {
      this
    } else {
      throw new IllegalArgumentException("Empty intersection")
    }

  def params = ???
}

abstract class CSPOMVariable(val params: Set[String]) extends SimpleExpression {
  def flattenVariables = Seq(this)
}

final case class CSPOMSeq[T <: CSPOMExpression](
  val values: Seq[T],
  val definedIndices: Range,
  val params: Set[String] = Set())
  extends Seq[T] with CSPOMExpression {

  require(values.nonEmpty)

  def this(seq: Seq[T]) = this(seq, seq.indices)

  //def variables = seq
  // Members declared in scala.collection.IterableLike 
  def iterator: Iterator[T] = values.iterator
  // Members declared in scala.collection.SeqLike 
  def apply(idx: Int): T = values(definedIndices.indexOf(idx))
  def length: Int = values.length
  def flattenVariables: Seq[CSPOMVariable] = values.flatMap(_.flattenVariables)

  def replaceVar(which: CSPOMExpression, by: CSPOMExpression) =
    new CSPOMSeq(values.map(_.replaceVar(which, by)), definedIndices, params)

  def intersected(that: CSPOMExpression) = throw new UnsupportedOperationException
  def contains(that: CSPOMConstant): Boolean = throw new UnsupportedOperationException
}

object CSPOMSeq {
  @annotation.varargs
  def apply(s: CSPOMExpression*) = new CSPOMSeq(s)
  @annotation.varargs
  def applyVar(s: CSPOMVariable*) = new CSPOMSeq(s)
}

object CSPOMExpression {

}

object CSPOMVariable {

  def aux() = new FreeVariable("var_is_introduced")

  def auxInt() = IntVariable.free("var_is_introduced")

  def auxBool() = new BoolVariable("var_is_introduced")

}