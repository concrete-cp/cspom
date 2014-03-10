package cspom.variable

import cspom.CSPOM
import scala.collection.mutable.WeakHashMap

/*
 * An expression can be either simple (a variable or a constant) or a sequence of expressions
 */
sealed trait CSPOMExpression[+T] extends Parameterized {

  def replaceVar[R >: T](which: CSPOMExpression[_ >: T], by: CSPOMExpression[R]): CSPOMExpression[R]

  def as(n: String)(implicit cspom: CSPOM): this.type = {
    cspom.nameExpression(this, n)
    this
  }

  def withName(n: String)(implicit cspom: CSPOM): (this.type, String) = {
    as(n)(cspom)
    (this, n)
  }
}

/*
 * Simple expressions are typed (int or boolean)
 */
sealed trait SimpleExpression[+T] extends CSPOMExpression[T] {
  final def replaceVar[R >: T](which: CSPOMExpression[_ >: T], by: CSPOMExpression[R]) =
    if (which == this) by else this

  def intersected(that: SimpleExpression[_ >: T]): SimpleExpression[T]

  def contains[S >: T](that: S): Boolean

}

class CSPOMConstant[+T](val value: T, val params: Set[Any] = Set()) extends SimpleExpression[T] {
  def contains[S >: T](that: S) = this == that

  def intersected(that: SimpleExpression[_ >: T]) =
    if (that.contains(value)) {
      CSPOMConstant(value, params ++ that.params)
    } else {
      throw new IllegalArgumentException("Empty intersection")
    }

  override def toString = value.toString

  override def equals(o: Any) = o match {
    case i: CSPOMConstant[_] => i.value == value
    case i: Any => i == value
  }

}

object CSPOMConstant {
  val cache = new WeakHashMap[(Any, Set[Any]), CSPOMConstant[Any]]

  cache.put((true, Set()), CSPOMTrue)
  cache.put((false, Set()), CSPOMFalse)

  def apply[A](value: A, params: Set[Any] = Set()): CSPOMConstant[A] =
    cache.getOrElseUpdate((value, params), new CSPOMConstant(value)).asInstanceOf[CSPOMConstant[A]]

  def unapply[A](c: CSPOMConstant[A]): Option[A] = Some(c.value)
}

abstract class CSPOMVariable[+T](val params: Set[Any]) extends SimpleExpression[T] {
  def flattenVariables = Seq(this)
}

final case class CSPOMSeq[+T](
  val values: Seq[CSPOMExpression[T]],
  val definedIndices: Range,
  val params: Set[Any] = Set())
  extends CSPOMExpression[T] with Seq[CSPOMExpression[T]] {

  def this(seq: Seq[CSPOMExpression[T]]) = this(seq, seq.indices)

  require(values.size == definedIndices.size)

  def iterator: Iterator[CSPOMExpression[T]] = values.iterator

  def withIndex = values zip definedIndices

  def apply(idx: Int): CSPOMExpression[T] = values(definedIndices.indexOf(idx))

  def length: Int = values.length

  def replaceVar[R >: T](which: CSPOMExpression[_ >: T], by: CSPOMExpression[R]) = {
    if (which == this) {
      by
    } else {
      val replaced = values.map(_.replaceVar(which, by))
      new CSPOMSeq(replaced, definedIndices, params)
    }
  }
}
