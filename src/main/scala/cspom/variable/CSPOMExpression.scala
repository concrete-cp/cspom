package cspom.variable

import cspom.CSPOM
import scala.collection.mutable.WeakHashMap
import javaexamples.queens.Queens

/*
 * An expression can be either a variable, a constant or a variable sequence
 */
trait CSPOMExpression {

  def ne(other: CSPOMExpression)(implicit problem: CSPOM) = problem.isReified("ne", this, other)

  def ≠(other: CSPOMExpression)(implicit problem: CSPOM) = ne(other)

  def ===(other: CSPOMExpression)(implicit problem: CSPOM) = problem.is("eq", this, other)

  def flattenVariables: Seq[CSPOMVariable]

  def cspomType: CSPOMType

  def replaceVar(which: CSPOMVariable, by: CSPOMExpression): CSPOMExpression
}

final case class CSPOMSeq(
  val name: String,
  val innerType: CSPOMType,
  val values: Seq[CSPOMExpression],
  val definedIndices: Range,
  val params: Set[String] = Set())
  extends Seq[CSPOMExpression] with CSPOMExpression {

  require(values.nonEmpty)
  require(values.forall(v => innerType.generalizes(v.cspomType)))

  def this(name: String, seq: Seq[CSPOMExpression]) = this(name, seq.head.cspomType, seq, seq.indices)
  def this(seq: Seq[CSPOMExpression]) = this(VariableNameGenerator.generate() + "_array", seq)

  //def variables = seq
  // Members declared in scala.collection.IterableLike 
  def iterator: Iterator[cspom.variable.CSPOMExpression] = values.iterator
  // Members declared in scala.collection.SeqLike 
  def apply(idx: Int): cspom.variable.CSPOMExpression = values(definedIndices.indexOf(idx))
  def length: Int = values.length
  def flattenVariables: Seq[CSPOMVariable] = values.flatMap(_.flattenVariables)
  def cspomType = CSPOMSeqType(innerType)
  def replaceVar(which: CSPOMVariable, by: CSPOMExpression) =
    new CSPOMSeq(name, innerType, values.map(_.replaceVar(which, by)), definedIndices, params)
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