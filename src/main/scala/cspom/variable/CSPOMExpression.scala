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
}

abstract class CSPOMVariable(val name: String, val params: Set[String]) extends SimpleExpression {
  require(params.nonEmpty)
  def this(name: String, params: String*) = this(name, params.toSet)

  def flattenVariables = Seq(this)

}

final case class CSPOMSeq[T <: CSPOMExpression](
  val name: String,
  val values: Seq[T],
  val definedIndices: Range,
  val params: Set[String] = Set())
  extends Seq[T] with CSPOMExpression {

  require(values.nonEmpty)

  def this(name: String, seq: Seq[T]) = this(name, seq, seq.indices)
  def this(seq: Seq[T]) = this(VariableNameGenerator.generate() + "_array", seq)

  //def variables = seq
  // Members declared in scala.collection.IterableLike 
  def iterator: Iterator[T] = values.iterator
  // Members declared in scala.collection.SeqLike 
  def apply(idx: Int): T = values(definedIndices.indexOf(idx))
  def length: Int = values.length
  def flattenVariables: Seq[CSPOMVariable] = values.flatMap(_.flattenVariables)

  def replaceVar(which: CSPOMExpression, by: CSPOMExpression) =
    new CSPOMSeq(name, values.map(_.replaceVar(which, by)), definedIndices, params)
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

object CSPOMVariable {

  /**
   * Constructs a new variable with a domain defined by lower
   * and upper bounds.
   *
   * @param <E>
   *            Type of bounds.
   * @param lB
   *            Lower bound of the domain
   * @param uB
   *            Upper bound of the domain
   */
  def ofInterval(name: String = VariableNameGenerator.generate, lb: Int, ub: Int, params: Set[String] = Set()) = {
    //val i = intervals.getOrElseUpdate((lb, ub), new IntInterval(lb, ub))

    new IntVariable(name, new IntInterval(lb, ub), params);
  }

  /**
   * Constructs a new variable with given name. Domain is defined by a list of
   * values.
   *
   * @param <T>
   *            Type of the values.
   * @param name
   *            Name of the variable.
   * @param values
   *            List of values defining the domain.
   */
  def ofInt(values: Int*) = ofIntSeq(values)

  def ofInt(name: String, values: Int*) = ofIntSeq(name, values)

  def ofIntSeq(name: String = VariableNameGenerator.generate(), values: Seq[Int], params: Set[String] = Set()): IntVariable =
    IntVariable.of(name, IntDomain.of(values: _*), params)

  def ofIntSeq(values: Seq[Int], params: String*): IntVariable =
    ofIntSeq(values = values, params = params.toSet)

  def bool(name: String = VariableNameGenerator.generate()) =
    new BoolVariable(name)

  def aux() = new FreeVariable(VariableNameGenerator.generate(), "var_is_introduced")

  def auxInt() = IntVariable.free(VariableNameGenerator.generate(), "var_is_introduced")

  def auxBool() = new BoolVariable(VariableNameGenerator.generate(), "var_is_introduced")

}