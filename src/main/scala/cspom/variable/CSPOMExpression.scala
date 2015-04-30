package cspom.variable

import scala.collection.SetLike
import scala.collection.mutable.WeakHashMap
import cspom.CSPOM
import cspom.Parameterized
import cspom.util.ContiguousIntRangeSet
import cspom.UNSATException
import scala.collection.mutable.HashMap
import scala.reflect.runtime.universe._

/*
 * An expression can be either simple (a variable or a constant) or a sequence of expressions
 */
sealed trait CSPOMExpression[+T] {

  implicit def tpe: Type

  def replaceVar[R >: T: TypeTag](which: CSPOMExpression[_ >: T], by: CSPOMExpression[R]): CSPOMExpression[R]

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

object CSPOMExpression {
  def unapply[T: TypeTag](c: CSPOMExpression[_]): Option[CSPOMExpression[T]] =
    if (c.tpe <:< typeOf[T]) {
      Some(c.asInstanceOf[CSPOMExpression[T]])
    } else {
      None
    }
}

/*
 * Simple expressions are typed (int or boolean)
 */
sealed trait SimpleExpression[+T] extends CSPOMExpression[T] {
  final def replaceVar[R >: T: TypeTag](which: CSPOMExpression[_ >: T], by: CSPOMExpression[R]) =
    if (which == this) by else this

  def intersected(that: SimpleExpression[_ >: T]): SimpleExpression[T]

  def contains[S >: T](that: S): Boolean

  def flatten = Seq(this)
}

object SimpleExpression {
  def iterable[A](e: SimpleExpression[A]): Iterable[A] = e match {
    case v: IntVariable      => new ContiguousIntRangeSet(v.domain)
    case b: BoolVariable     => Iterable(false, true)
    case CSPOMConstant(c)    => Iterable[A](c)
    case _: CSPOMVariable[A] => throw new IllegalArgumentException(s"Cannot iterate over $e")
  }

  class Typed[T: TypeTag] {
    def unapply(c: CSPOMExpression[_]): Option[SimpleExpression[T]] = Some(c).collect {
      case c: SimpleExpression[_] if (c.tpe <:< typeOf[T]) =>
        c.asInstanceOf[SimpleExpression[T]]
    }

  }

}

case class CSPOMConstant[+T: TypeTag](value: T) extends SimpleExpression[T] {
  require(!value.isInstanceOf[CSPOMExpression[_]])

  def tpe = typeOf[T]

  def contains[S >: T](that: S) = value == that

  def intersected(that: SimpleExpression[_ >: T]) = {
    if (that.contains(value)) {
      this
    } else {
      throw new UNSATException("Empty intersection")
    }
  }

  override def toString = s"[$value]"

  //  override def equals(o: Any) = o match {
  //    case i: CSPOMConstant[_] => i.value == value && i.params == params
  //    case i: Any              => i == value && params.isEmpty
  //  }
  //
  //  override def hashCode = 31 * value.hashCode + params.hashCode

  def isTrue = value == true

  def isFalse = value == false

  def fullyDefined = true

  def searchSpace = 1
}

//object CSPOMConstant {
//  val cache = new HashMap[Any, CSPOMConstant[Any]]
//
//  def apply[A](value: A): CSPOMConstant[A] =
//    cache.getOrElseUpdate(value, new CSPOMConstant(value)).asInstanceOf[CSPOMConstant[A]]
//
//  def unapply[A](c: CSPOMConstant[A]): Option[A] = Some(c.value)
//}

abstract class CSPOMVariable[+T: TypeTag]() extends SimpleExpression[T] {
  def tpe = typeOf[T]
  def flattenVariables = Seq(this)
  def isTrue = false
  def isFalse = false
}

object CSPOMSeq {
  def empty: CSPOMSeq[Nothing] = CSPOMSeq()
  @annotation.varargs
  def apply[T: TypeTag](seq: CSPOMExpression[T]*): CSPOMSeq[T] = CSPOMSeq(seq.toIndexedSeq, seq.indices)
  def apply[T: TypeTag](seq: IndexedSeq[CSPOMExpression[T]], indices: Range): CSPOMSeq[T] = new CSPOMSeq(seq, indices)

  def unapply[A](s: CSPOMSeq[A]): Option[Seq[CSPOMExpression[A]]] =
    Some(s.values)
}

final class CSPOMSeq[+T: TypeTag](
  val values: IndexedSeq[CSPOMExpression[T]],
  val definedIndices: Range)
  extends CSPOMExpression[T] with Seq[CSPOMExpression[T]] {

  def tpe = typeOf[T]

  def +:[S >: T: TypeTag](v: CSPOMExpression[S]) = CSPOMSeq(v +: values: _*)

  def :+[S >: T: TypeTag](v: CSPOMExpression[S]) = CSPOMSeq(values :+ v: _*)

  require(values.size == definedIndices.size)

  def iterator: Iterator[CSPOMExpression[T]] = values.iterator

  def withIndex = values zip definedIndices

  def apply(idx: Int): CSPOMExpression[T] = values(definedIndices.indexOf(idx))

  def length: Int = values.length

  def replaceVar[R >: T: TypeTag](which: CSPOMExpression[_ >: T], by: CSPOMExpression[R]) = {
    if (which == this) {
      by
    } else {
      val replaced = values.map(_.replaceVar(which, by))
      if (values == replaced) {
        this
      } else {
        new CSPOMSeq(replaced, definedIndices)
      }
    }
  }

  def replaceIndex[R >: T: TypeTag](index: Int, by: CSPOMExpression[R]) = {
    val realIndex = definedIndices.indexOf(index)
    new CSPOMSeq(values.updated(realIndex, by), definedIndices)
  }

  def flatten = values.flatMap(_.flatten)

  def isTrue = false

  def isFalse = false

  def fullyDefined = values.forall(_.fullyDefined)

  def searchSpace = values.foldLeft(1.0)(_ * _.searchSpace)

  def zipWithIndex = values.iterator.zip(definedIndices.iterator)
  //
  //  override def equals(o: Any) = o match {
  //    case a: AnyRef => a eq this
  //    case _         => false
  //  }
  //
  //  override def hashCode() = System.identityHashCode(this)
}
