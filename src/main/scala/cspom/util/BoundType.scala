package cspom.util

import com.google.common.collect.{ BoundType => GuavaBT }

object BoundType {
  def apply(t: GuavaBT) = t match {
    case GuavaBT.CLOSED => Closed
    case GuavaBT.OPEN => Open
  }

  val closedIsLess = Ordering.fromLessThan[BoundType] {
    case (Closed, Open) => true
    case _ => false
  }

  val closedIsMore = Ordering.fromLessThan[BoundType] {
    case (Open, Closed) => true
    case _ => false
  }
}

sealed trait BoundType {
  def other: BoundType
  def guava: GuavaBT
  def &(bt: BoundType): BoundType
}

case object Closed extends BoundType {
  def other = Open
  def guava = GuavaBT.CLOSED
  def &(bt: BoundType) = bt match {
    case Closed => Closed
    case Open => Open
  }
}
case object Open extends BoundType {
  def other = Closed
  def guava = GuavaBT.OPEN
  def &(bt: BoundType) = Open
}