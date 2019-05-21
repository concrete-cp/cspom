package cspom
package variable

import com.typesafe.scalalogging.LazyLogging
import cspom.util.ContiguousIntRangeSet

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._

/*
 * An expression can be either simple (a variable or a constant) or a sequence of expressions
 */
sealed trait CSPOMExpression[+T] {

  implicit def tpe: Type

  def replaceVar[R >: T : TypeTag](which: CSPOMExpression[_ >: T], by: CSPOMExpression[R]): CSPOMExpression[R]

  def withName(n: String)(implicit cspom: CSPOM): (this.type, String) = {
    as(n)(cspom)
    (this, n)
  }

  def as(n: String)(implicit cspom: CSPOM): this.type = {
    cspom.nameExpression(this, n)
    this
  }

  def ≠(other: CSPOMExpression[_ >: T])(implicit problem: CSPOM): SimpleExpression[Boolean] =
    this !== other

  def !==(other: CSPOMExpression[_ >: T])(implicit problem: CSPOM): SimpleExpression[Boolean] = {
    problem.defineBool(result => CSPOMConstraint(result)('ne)(this, other))
  }

  def ===(other: CSPOMExpression[_ >: T])(implicit problem: CSPOM): SimpleExpression[Boolean] =
    problem.defineBool(result => CSPOMConstraint(result)('eq)(this, other))

  def flatten: Seq[SimpleExpression[T]]

  def flattenVariables: Seq[CSPOMVariable[T]]

  def isTrue: Boolean

  def isFalse: Boolean

  def fullyDefined: Boolean

  def searchSpace: Double

  def isConstant: Boolean

  def toString(names: CSPOMExpression[_] => String): String
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
  final def replaceVar[R >: T : TypeTag](which: CSPOMExpression[_ >: T], by: CSPOMExpression[R]): CSPOMExpression[R] =
    if (which == this) by else this

  def intersected(that: SimpleExpression[_ >: T]): SimpleExpression[T]

  def contains[S >: T](that: S): Boolean

  def flatten: Seq[SimpleExpression[T]] = Seq(this)

  def isEmpty: Boolean

//
//  def set: Set[T] = new Set[T] {
//    def contains(i: T): Boolean = SimpleExpression.this.contains(i)
//
//    override def +(elem: T): Set[T] = ???
//
//    override def -(elem: T): Set[T] = ???
//
//    override def iterator: Iterator[T] = ???
//  }
}

object SimpleExpression {
  def iterable[A](e: SimpleExpression[A]): Iterable[A] = e match {
    case v: IntVariable => new ContiguousIntRangeSet(v.domain)
    case _: BoolVariable => Iterable(false, true)
    case CSPOMConstant(c) => Iterable[A](c)
    case _: CSPOMVariable[A] => throw new IllegalArgumentException(s"Cannot iterate over $e")
  }

  class Typed[T: TypeTag] {
    def unapply(c: CSPOMExpression[_]): Option[SimpleExpression[T]] = PartialFunction.condOpt(c) {
      case c: SimpleExpression[_] if c.tpe <:< typeOf[T] => c.asInstanceOf[SimpleExpression[T]]
    }

  }

  object simpleCSPOMSeq {
    def unapply[A: TypeTag](e: CSPOMExpression[A]): Option[Seq[SimpleExpression[A]]] =
      PartialFunction.condOpt(e) {
        case s: CSPOMSeq[A] => s
      }
        .flatMap { cspomSeq =>
          CSPOMSeq.collectAll(cspomSeq) {
            case c: SimpleExpression[A] if c.tpe <:< typeOf[A] => c.asInstanceOf[SimpleExpression[A]]
          }
        }
  }

  object simpleSeq {
    def unapply[A: TypeTag](e: Seq[CSPOMExpression[A]]): Option[IndexedSeq[SimpleExpression[A]]] =
      CSPOMSeq.collectAll(e) {
        case c: SimpleExpression[A] => c
      }
        .map(_.toIndexedSeq)
  }

}

object CSPOMConstant {


  val cache = new mutable.WeakHashMap[Any, CSPOMConstant[Any]]

  def apply[T: TypeTag](value: T): CSPOMConstant[T] = {
    cache.getOrElseUpdate(value, new CSPOMConstant(value)).asInstanceOf[CSPOMConstant[T]]
  }

  def unapply[A](c: CSPOMConstant[A]): Option[A] = Some(c.value)


  object seq {
    def unapply[T: TypeTag](e: CSPOMExpression[T]): Option[Seq[T]] = e match {
      case s: CSPOMSeq[T] => CSPOMSeq.collectAll(s) {
        case CSPOMConstant(v) => v
      }
      case _ => None
    }
  }

  //
  //  def ofSeq[T: TypeTag](s: Seq[T]): CSPOMSeq[T] = CSPOMSeq(s.map(CSPOMConstant(_)), 0 until s.size)
}

class CSPOMConstant[+T: TypeTag](val value: T) extends SimpleExpression[T] {
  require(!value.isInstanceOf[CSPOMExpression[_]])

  def tpe: Type = typeOf[T]

  def contains[S >: T](that: S): Boolean = bool2int(value) == bool2int(that)

  private def bool2int(b: Any): Any = b match {
    case true => 1
    case false => 0
    case e => e

  }

  def intersected(that: SimpleExpression[_>:T]): SimpleExpression[T] = {
    if (that.contains(value)) {
      this
    } else {
      EmptyVariable
    }
  }

  def isTrue: Boolean = value == true || value == 1

  override def equals(o: Any): Boolean = o match {
    case i: CSPOMConstant[_] => bool2int(i.value) == bool2int(value)
    case _ => false
  }

  override def hashCode: Int = 31 * bool2int(value).hashCode

  def isFalse: Boolean = value == false || value == 0

  def fullyDefined = true

  def searchSpace = 1

  def isEmpty = false

  def isConstant = true

  def flattenVariables: Seq[CSPOMVariable[T]] = Seq()

  def toString(names: CSPOMExpression[_] => String): String = toString

  override def toString: String = value.toString

  def intValue: Int = bool2int(value) match {
    case i: Int => i
    case _ => throw new ClassCastException(s"Cannot convert $this to int")
  }
}

abstract class CSPOMVariable[+T: TypeTag]() extends SimpleExpression[T] {
  def tpe: Type = typeOf[T]

  def flattenVariables: Seq[CSPOMVariable[T]] = Seq(this)

  def isTrue = false

  def isFalse = false

  def isConstant = false

  def toString(names: CSPOMExpression[_] => String) = s"${names(this)}=${toString()}"
}

object CSPOMSeq {

  private val cache = new mutable.WeakHashMap[CSPOMSeq[_], CSPOMSeq[_]]

  lazy val empty: CSPOMSeq[Nothing] = new CSPOMSeq(IndexedSeq.empty, IndexedSeq.empty.indices)
  // @annotation.varargs def apply[T: TypeTag](seq: CSPOMExpression[T]*): CSPOMSeq[T] = CSPOMSeq(seq.toIndexedSeq, seq.indices)

  def apply[T: TypeTag](seq: CSPOMExpression[T]*): CSPOMSeq[T] =
    apply(seq, seq.indices)

  def apply[T: TypeTag](seq: Seq[CSPOMExpression[T]], indices: Range): CSPOMSeq[T] =
    if (seq.isEmpty) empty else {
      val newSeq = new CSPOMSeq(seq.toIndexedSeq, indices)
      cache.getOrElseUpdate(newSeq, newSeq).asInstanceOf[CSPOMSeq[T]]
    }


  def unapply[A](s: CSPOMSeq[A]): Option[IndexedSeq[CSPOMExpression[A]]] =
    Some(s.values)

  def collectAll[A, B](s: Seq[A])(f: PartialFunction[A, B]): Option[Seq[B]] = {
    val r = new ArrayBuffer[B](s.length)
    for (h <- s) {
      if (f.isDefinedAt(h)) {
        r += f(h)
      } else {
        return None
      }
    }
    Some(r.result())
  }

  def searchSpace(s: Seq[CSPOMExpression[_]]): Double = {
    s.foldLeft(1.0)(_ * _.searchSpace)
  }

}

final class CSPOMSeq[+T: TypeTag] private (
                                   val values: IndexedSeq[CSPOMExpression[T]],
                                   val definedIndices: Range)
  extends CSPOMExpression[T] with IndexedSeq[CSPOMExpression[T]] with LazyLogging {

  //  if (definedIndices.headOption.contains(0)) {
  //    logger.info(s"$this is 0-indexed CSPOMSeq")
  //  }

  lazy val flatten: IndexedSeq[SimpleExpression[T]] = values.flatMap(_.flatten)
  lazy val fullyDefined: Boolean = values.forall(_.fullyDefined)
  lazy val isConstant: Boolean = values.forall(_.isConstant)

  require(values.size == definedIndices.size)
  lazy val flattenVariables: IndexedSeq[CSPOMVariable[T]] = values.flatMap(_.flattenVariables)
  override lazy val hashCode: Int = super.hashCode



  def tpe: universe.Type = typeOf[T]

  def +:[S >: T : TypeTag](v: CSPOMExpression[S]) = CSPOMSeq(v +: values, definedIndices.head - 1 to definedIndices.last)

  def :+[S >: T : TypeTag](v: CSPOMExpression[S]) = CSPOMSeq(values :+ v, definedIndices.head to definedIndices.last + 1)

  override def iterator: Iterator[CSPOMExpression[T]] = values.iterator

  def withIndex: IndexedSeq[(CSPOMExpression[T], Int)] = values zip definedIndices

  def apply(idx: Int): CSPOMExpression[T] = {
    val index = definedIndices.indexOf(idx)
    require(index >= 0, s"$idx out of range $definedIndices")
    values(index)
  }

  def length: Int = values.length

  def replaceVar[R >: T : TypeTag](which: CSPOMExpression[_ >: T], by: CSPOMExpression[R]): CSPOMExpression[R] = {
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

  def replaceIndex[R >: T : TypeTag](index: Int, by: CSPOMExpression[R]): CSPOMSeq[R] = {
    val realIndex = definedIndices.indexOf(index)
    new CSPOMSeq(values.updated(realIndex, by), definedIndices)
  }

  def isTrue = false

  def isFalse = false

  def searchSpace: Double = CSPOMSeq.searchSpace(values)

  def zipWithIndex: Iterator[(CSPOMExpression[T], Int)] = values.iterator.zip(definedIndices.iterator)

  override def equals(o: Any): Boolean = o match {
    case a: CSPOMSeq[_] => a.values == values && a.definedIndices == definedIndices
    case _ => false
  }

  def toString(names: CSPOMExpression[_] => String): String = {
    if (isEmpty) {
      s"${names(this)}: CSPOMSeq()"
    } else if (indices.head != 0) {
      s"${names(this)}: CSPOMSeq[${indices.head}..${indices.last}](${values.map(_.toString(names)).mkString(", ")})"
    } else {
      s"${names(this)}: CSPOMSeq(${values.map(_.toString(names)).mkString(", ")})"
    }
  }

  //
  //  override def equals(o: Any) = o match {
  //    case a: AnyRef => a eq this
  //    case _         => false
  //  }
  //
  //  override def hashCode() = System.identityHashCode(this)
}
