package cspom.compiler

import scala.collection.immutable.BitSet
import cspom.util.BitVector

object QueueSet {
  val empty: QueueSet = QueueSet(BitVector.empty, -1)
  def apply(init: Traversable[Int]): QueueSet =
    QueueSet(BitVector(init), -1)
}

case class QueueSet(present: BitVector, first: Int) {

  def enqueue(c: Int): QueueSet = {
    new QueueSet(present + c, first)
  }

  def dequeue(): (Int, QueueSet) = {
    require(nonEmpty)
    val next = present.nextSetBit(first + 1) match {
      case n if n < 0 => present.nextSetBit(0)
      case n          => n
    }

    (next, new QueueSet(present - next, next))
  }

  def contains(e: Int): Boolean = present(e)

  def nonEmpty: Boolean = !present.isEmpty

  def enqueueAll(init: BitVector): QueueSet = {
    QueueSet(present | init, first)
  }

  def enqueueAll(init: Traversable[Int]): QueueSet = {
    QueueSet(present ++ init, first)
  }

  def remove(e: Int*): QueueSet = removeAll(e)

  def removeAll(e: Iterable[Int]): QueueSet = {
    QueueSet(present -- e, first)
  }

}