package cspom.variable

import cspom.CSPOM
import scala.collection.mutable.WeakHashMap
import cspom.Parameterized
import scala.collection.SetLike

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
  
  def fullyDefined: Boolean
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

  def domain: Iterable[T]

}

class CSPOMConstant[+T](val value: T, val params: Map[String, Any] = Map()) extends SimpleExpression[T] {
  require(!value.isInstanceOf[CSPOMExpression[_]])

  def contains[S >: T](that: S) = value == that

  def intersected(that: SimpleExpression[_ >: T]) =
    if (that.contains(value)) {
      CSPOMConstant(value, Map("intersection" -> (this, that)))
    } else {
      throw new IllegalArgumentException("Empty intersection")
    }

  override def toString = value.toString + displayParams

  override def equals(o: Any) = o match {
    case i: CSPOMConstant[_] => i.value == value && i.params == params
    case i: Any => i == value && params.isEmpty
  }

  def isTrue = value == true

  def domain = Iterable(value)
  
  def fullyDefined = true
}

object CSPOMConstant {
  val cache = new WeakHashMap[(Any, Map[String, Any]), CSPOMConstant[Any]]

  def apply[A](value: A, params: Map[String, Any] = Map()): CSPOMConstant[A] =
    cache.getOrElseUpdate((value, params), new CSPOMConstant(value, params)).asInstanceOf[CSPOMConstant[A]]

  def unapply[A](c: CSPOMConstant[A]): Option[A] = Some(c.value)
}

//object CSPOMConstant(true) extends CSPOMConstant(true)
//
//object CSPOMConstant(false) extends CSPOMConstant(false)

abstract class CSPOMVariable[+T](val params: Map[String, Any]) extends SimpleExpression[T] {
  def flattenVariables = Seq(this)
  def isTrue = false
}

final case class CSPOMSeq[+T](
  val values: Seq[CSPOMExpression[T]],
  val definedIndices: Range,
  val params: Map[String, Any] = Map())
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

  def flatten = values.flatMap(_.flatten)

  def isTrue = false
  
  def fullyDefined = values.forall(_.fullyDefined)
}
