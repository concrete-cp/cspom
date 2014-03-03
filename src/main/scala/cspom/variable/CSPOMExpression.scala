package cspom.variable

import cspom.CSPOM
import scala.collection.mutable.WeakHashMap

/*
 * An expression can be either simple (a variable or a constant) or a sequence of expressions
 */
sealed trait CSPOMExpression[+T] {

  def replaceVar[R >: T](which: CSPOMExpression[_ >: T], by: CSPOMExpression[R]): CSPOMExpression[R]

  def params: Set[Any]

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

class CSPOMConstant[+T](val value: T) extends SimpleExpression[T] {
  def contains[S >: T](that: S) = this == that

  def intersected(that: SimpleExpression[_ >: T]) =
    if (that.contains(value)) {
      this
    } else {
      throw new IllegalArgumentException("Empty intersection")
    }

  override def toString = value.toString

  override def equals(o: Any) = o match {
    case i: CSPOMConstant[_] => i.value == value
    case i: Any => i == value
  }

  def params = ???
}

object CSPOMConstant {
  val cache = new WeakHashMap[Any, CSPOMConstant[Any]]
  
  cache.put(true, CSPOMTrue)
  cache.put(false, CSPOMFalse)

  def apply[A](value: A): CSPOMConstant[A] =
    cache.getOrElseUpdate(value, new CSPOMConstant(value)).asInstanceOf[CSPOMConstant[A]]

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

  //def variables = seq
  // Members declared in scala.collection.IterableLike 
  def iterator: Iterator[CSPOMExpression[T]] = values.iterator

  def withIndex = values zip definedIndices

  // Members declared in scala.collection.SeqLike 
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

  List(1, 2)
}

//object CSPOMSeq {
//  @annotation.varargs
//  def apply[A](s: CSPOMExpression[A]*) = new CSPOMSeq[A](s)
//  @annotation.varargs
//  def applyVar[T](s: CSPOMExpression[T]*) = new CSPOMSeq(s)
//}

object CSPOMVariable {

  def aux() = new FreeVariable("var_is_introduced")

  def auxInt() = IntVariable.free("var_is_introduced")

  def auxBool() = new BoolVariable("var_is_introduced")

}