package cspom.variable

import scala.collection.SetLike
import scala.collection.mutable.WeakHashMap
import cspom.CSPOM
import cspom.Parameterized
import cspom.util.ContiguousIntRangeSet

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

  def !==(other: CSPOMExpression[_ >: T])(implicit problem: CSPOM): BoolVariable = problem.isBool('ne, Seq(this, other))

  def ≠(other: CSPOMExpression[_ >: T])(implicit problem: CSPOM): BoolVariable = this !== other

  def ===(other: CSPOMExpression[_ >: T])(implicit problem: CSPOM): BoolVariable = problem.isBool('eq, Seq(this, other))

  def flatten: Seq[SimpleExpression[T]]

  def isTrue: Boolean

  def isFalse: Boolean

  def fullyDefined: Boolean

  def searchSpace: Double
}

/*
 * Simple expressions are typed (int or boolean)
 */
sealed trait SimpleExpression[+T] extends CSPOMExpression[T] {
  final def replaceVar[R >: T](which: CSPOMExpression[_ >: T], by: CSPOMExpression[R]) =
    if (which == this) by else this

  def intersected(that: SimpleExpression[_ >: T]): SimpleExpression[T]

  def contains[S >: T](that: S): Boolean

  def flatten = Seq(this)
}

object SimpleExpression {
  def iterable[A](e: SimpleExpression[A]): Iterable[A] = e match {
    case v: IntVariable   => new ContiguousIntRangeSet(v.domain)
    case b: BoolVariable  => Iterable(false, true)
    case CSPOMConstant(c) => Iterable[A](c)

  }

}

class CSPOMConstant[+T](val value: T, val params: Map[String, Any] = Map()) extends SimpleExpression[T] {
  require(!value.isInstanceOf[CSPOMExpression[_]])

  def contains[S >: T](that: S) = value == that

  def intersected(that: SimpleExpression[_ >: T]) = {
    require(that.contains(value), "Empty intersection")
    CSPOMConstant(value, Map("intersection" -> ((this, that))))
  }

  override def toString = s"[$value]$displayParams"

  override def equals(o: Any) = o match {
    case i: CSPOMConstant[_] => i.value == value && i.params == params
    case i: Any              => i == value && params.isEmpty
  }

  override def hashCode = 31 * value.hashCode + params.hashCode

  def isTrue = value == true

  def isFalse = value == false

  def fullyDefined = true

  def searchSpace = 1
}

object CSPOMConstant {
  val cache = new WeakHashMap[(Any, Map[String, Any]), CSPOMConstant[Any]]

  def apply[A](value: A, params: Map[String, Any] = Map()): CSPOMConstant[A] =
    cache.getOrElseUpdate((value, params), new CSPOMConstant(value, params)).asInstanceOf[CSPOMConstant[A]]

  def unapply[A](c: CSPOMConstant[A]): Option[A] = Some(c.value)
}

abstract class CSPOMVariable[+T](val params: Map[String, Any]) extends SimpleExpression[T] {
  def flattenVariables = Seq(this)
  def isTrue = false
  def isFalse = false
}

object CSPOMSeq {
  def empty: CSPOMSeq[Nothing] = CSPOMSeq()
  def apply[T](seq: CSPOMExpression[T]*): CSPOMSeq[T] = CSPOMSeq(seq, seq.indices)
  def apply[T](seq: Seq[CSPOMExpression[T]], indices: Range): CSPOMSeq[T] = new CSPOMSeq(seq, indices)

  def unapply[A](s: CSPOMSeq[A]): Option[Seq[CSPOMExpression[A]]] =
    Some(s.values)
}

final class CSPOMSeq[+T](
  val values: Seq[CSPOMExpression[T]],
  val definedIndices: Range,
  val params: Map[String, Any] = Map())
  extends CSPOMExpression[T] with Seq[CSPOMExpression[T]] {

  def +:[S >: T](v: CSPOMExpression[S]) = CSPOMSeq(v +: values: _*)

  def :+[S >: T](v: CSPOMExpression[S]) = CSPOMSeq(values :+ v: _*)

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

  def flatten = values.flatMap(_.flatten)

  def isTrue = false

  def isFalse = false

  def fullyDefined = values.forall(_.fullyDefined)

  def searchSpace = values.foldLeft(0.0)(_ * _.searchSpace)

  def zipWithIndex = values.iterator.zip(definedIndices.iterator)
}
