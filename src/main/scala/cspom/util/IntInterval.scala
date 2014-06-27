package cspom.util

import com.google.common.collect.{ BoundType => GuavaBT }
import com.google.common.collect.ContiguousSet
import com.google.common.collect.Range
import com.typesafe.scalalogging.slf4j.LazyLogging
import com.google.common.collect.DiscreteDomain
import scala.collection.JavaConversions
import scala.collection.AbstractIterator

object IntInterval {
  val all: IntInterval = IntInterval(MinInf, PlusInf)

  def atLeast(endpoint: Int) = IntInterval(Finite(endpoint), PlusInf)

  def atMost(endpoint: Int) = IntInterval(MinInf, Finite(endpoint))

  def apply(lower: Int, upper: Int): IntInterval =
    IntInterval(Finite(lower), Finite(upper))

  def singleton(v: Int): IntInterval = IntInterval(v, v)

}

//sealed trait IntBound extends Ordered[IntBound]
//sealed trait LowerIntBound extends IntBound {
//  def <=(c: Int): Boolean
//}
//case object NoLowerBound extends LowerIntBound {
//  def <=(c: Int) = true
//  def compare(c: IntBound) = -1
//  override def toString = "(-\u221e"
//}
//case class AtLeast(i: Int) extends LowerIntBound {
//  def <=(c: Int) = i <= c
//  def compare(c: IntBound) = c match {
//    case NoLowerBound => 1
//    case NoUpperBound => -1
//    case AtLeast(j) => i.compare(j)
//    case AtMost(j) => i.compare(j)
//  }
//  override def toString = s"[$i"
//}
//
//sealed trait UpperIntBound extends IntBound {
//  def >=(c: Int): Boolean
//}
//case object NoUpperBound extends UpperIntBound {
//  def >=(c: Int) = true
//  def compare(c: IntBound) = 1
//  override def toString = "+-\u221e)"
//}
//case class AtMost(i: Int) extends UpperIntBound {
//  def >=(c: Int) = i >= c
//  def compare(c: IntBound) = c match {
//    case NoLowerBound => 1
//    case NoUpperBound => -1
//    case AtMost(j) => i.compare(j)
//    case AtMost(j) => i.compare(j)
//  }
//  override def toString = s"$i]"
//}

final case class IntInterval(
  val lb: Infinitable, val ub: Infinitable) extends LazyLogging {

  require(lb != PlusInf)
  require(ub != MinInf)

  def contains(c: Int) = lb <= c && !(ub < c)

  def &(si: IntInterval) = {
    val lowerCmp = lb.compare(si.lb);
    val upperCmp = ub.compare(si.ub);
    if (lowerCmp >= 0 && upperCmp <= 0) {
      this
    } else if (lowerCmp <= 0 && upperCmp >= 0) {
      si
    } else {
      val newLower = if (lowerCmp >= 0) lb else si.lb
      val newUpper = if (upperCmp <= 0) ub else si.ub
      IntInterval(newLower, newUpper)
    }
  }

  def isConnected(si: IntInterval) = {
    (lb <= si.ub || (lb > Finite(Int.MinValue) && lb - Finite(1) <= si.ub)) &&
      ((si.lb <= ub) || (si.lb > Finite(Int.MinValue) && si.lb - Finite(1) <= ub))
  }

  def span(si: IntInterval): IntInterval = {
    val lowerCmp = lb.compare(si.lb);
    val upperCmp = ub.compare(si.ub);
    if (lowerCmp <= 0 && upperCmp >= 0) {
      this
    } else if (lowerCmp >= 0 && upperCmp <= 0) {
      si
    } else {
      val newLower = if (lowerCmp <= 0) lb else si.lb
      val newUpper = if (upperCmp >= 0) ub else si.ub
      IntInterval(newLower, newUpper);
    }
  }

  def isEmpty = lb.compare(ub) > 0

  def isBefore(h: IntInterval): Boolean = {
    h.lb match {
      case MinInf => false
      case Finite(i) => isBefore(i)
      case PlusInf => throw new IllegalArgumentException
    }
  }

  def isBefore(elem: Int): Boolean = {
    ub match {
      case PlusInf => false
      case Finite(i) => i < elem
      case MinInf => throw new IllegalArgumentException
    }
  }

  def isAfter(h: IntInterval): Boolean = {
    h.ub match {
      case PlusInf => false
      case Finite(i) => isAfter(i)
      case MinInf => throw new IllegalArgumentException
    }
  }

  def isAfter(elem: Int): Boolean = {
    lb match {
      case MinInf => false
      case Finite(i) => i > elem
      case PlusInf => throw new IllegalArgumentException
    }
  }

  def allValues: Iterator[Int] = {
    val Finite(l) = lb
    val Finite(u) = ub
    new AbstractIterator[Int] {
      private var i = l
      def hasNext: Boolean = i <= u
      def next(): Int =
        if (hasNext) { val result = i; i += 1; result }
        else Iterator.empty.next()
    }
  }

  def nbValues: Int = {
    val Finite(s) = (ub - lb) + Finite(1)
    s
  }

  override def toString = {
    val l = lb match {
      case MinInf => "(-\u221e"
      case Finite(i) => s"[$i"
      case PlusInf => throw new AssertionError
    }
    val u = ub match {

      case Finite(i) => s"$i]"
      case PlusInf => "+\u221e)"
      case MinInf => throw new AssertionError
    }

    s"$l‥$u"
  }

}