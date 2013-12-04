package cspom.variable

import cspom.CSPOM
import scala.collection.mutable.WeakHashMap

/*
 * An expression can be either a variable, a constant or a variable sequence
 */
trait CSPOMExpression {
  def flattenVariables: Seq[CSPOMVariable]

  def cspomType: CSPOMType

  def replaceVar(which: CSPOMVariable, by: CSPOMExpression): CSPOMExpression

  def intersected(that: CSPOMExpression): CSPOMExpression

  def contains(that: CSPOMConstant): Boolean
}

trait SimpleExpression extends CSPOMExpression

trait IntExpression extends SimpleExpression

trait BoolExpression extends SimpleExpression {
  def neg: BoolExpression
}

final case class CSPOMSeq[T <: CSPOMExpression](
  val name: String,
  val innerType: CSPOMType,
  val values: Seq[T],
  val definedIndices: Range,
  val params: Set[String] = Set())
  extends Seq[T] with CSPOMExpression {

  require(values.nonEmpty)
  require(values.forall(v => innerType.generalizes(v.cspomType)))

  def this(name: String, seq: Seq[T]) = this(name, seq.head.cspomType, seq, seq.indices)
  def this(seq: Seq[T]) = this(VariableNameGenerator.generate() + "_array", seq)

  //def variables = seq
  // Members declared in scala.collection.IterableLike 
  def iterator: Iterator[T] = values.iterator
  // Members declared in scala.collection.SeqLike 
  def apply(idx: Int): T = values(definedIndices.indexOf(idx))
  def length: Int = values.length
  def flattenVariables: Seq[CSPOMVariable] = values.flatMap(_.flattenVariables)
  def cspomType = CSPOMSeqType(innerType)
  def replaceVar(which: CSPOMVariable, by: CSPOMExpression) =
    new CSPOMSeq(name, innerType, values.map(_.replaceVar(which, by)), definedIndices, params)
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
  def valueOf(desc: String): IntExpression = {
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