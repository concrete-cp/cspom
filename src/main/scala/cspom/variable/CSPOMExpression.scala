package cspom.variable

import cspom.CSPOM
import scala.collection.mutable.WeakHashMap

/*
 * An expression can be either a variable, a constant or a variable sequence
 */
trait CSPOMExpression {

  def ne(other: CSPOMExpression)(implicit problem: CSPOM) = problem.isReified("ne", this, other)

  def ≠(other: CSPOMExpression)(implicit problem: CSPOM) = ne(other)

  def ==(other: CSPOMExpression)(implicit problem: CSPOM) = problem.is("eq", this, other)

  def flattenVariables: Seq[CSPOMVariable]
}

final case class CSPOMSeq(
  val name: String,
  val values: Seq[CSPOMExpression],
  val definedIndices: Range,
  val params: Seq[String] = Seq())
  extends Seq[CSPOMExpression] with CSPOMExpression {
  def this(name: String, seq: Seq[CSPOMExpression]) = this(name, seq, seq.indices)
  def this(seq: Seq[CSPOMExpression]) = this(VariableNameGenerator.generate() + "_array", seq)
  //def variables = seq
  // Members declared in scala.collection.IterableLike 
  def iterator: Iterator[cspom.variable.CSPOMExpression] = values.iterator
  // Members declared in scala.collection.SeqLike 
  def apply(idx: Int): cspom.variable.CSPOMExpression = values(definedIndices.indexOf(idx))
  def length: Int = values.length
  def flattenVariables: Seq[CSPOMVariable] = values.flatMap(_.flattenVariables)
}

trait CSPOMConstant extends CSPOMExpression {
  def flattenVariables = Seq()
}

final class IntConstant private (val value: Int) extends CSPOMConstant {
  override def toString = value.toString
}

object IntConstant {
  val cache = new WeakHashMap[Int, IntConstant]
  def apply(value: Int) =
    cache.getOrElseUpdate(value, new IntConstant(value))

}

final class DoubleConstant private (val value: Double) extends CSPOMConstant {
  override def toString = value.toString
}

object DoubleConstant {
  val cache = new WeakHashMap[Double, DoubleConstant]
  def apply(value: Double) =
    cache.getOrElseUpdate(value, new DoubleConstant(value))

}

object CSPOMTrue extends CSPOMConstant {
  override def toString = "true"
}

object CSPOMFalse extends CSPOMConstant {
  override def toString = "false"
}

object CSPOMExpression {
  /**
   * Parse the given expression given as a String. Domains are usually sequence of
   * values separated by spaces such as "1 3 -4 5" or intervals in the format
   * "a..b". Sequences of values and intervals such as "1 3..10 18..30" are
   * allowed and converted to a sequence of values.
   *
   * @param domain
   *            The String domain to parse
   * @return The resulting Domain object
   */
  def valueOf(desc: String): CSPOMExpression = {
    val values: Seq[Int] = desc.trim.split(" +").flatMap { v =>
      if (v.contains("..")) {
        IntInterval.valueOf(v);
      } else {
        List(v.trim.toInt);
      }
    }

    if (values.size == 1) {
      IntConstant(values.head)
    } else {
      CSPOMVariable.ofInt(values: _*)
    }

  }
}