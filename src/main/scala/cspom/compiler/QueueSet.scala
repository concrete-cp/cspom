package cspom.compiler

import scala.collection.mutable.Queue
import scala.collection.mutable.HashSet
import java.util.BitSet

final class QueueSet {
  private val present = new BitSet()

  private var first = -1

  //def enqueue(e: A*): Unit = e.foreach(enqueue)

  def this(init: Iterable[Int]) = {
    this()
    enqueueAll(init)
  }

  def enqueue(c: Int): Unit = {
    present.set(c)
  }

  def dequeue(): Int = {
    assume(nonEmpty)
    first = present.nextSetBit(first + 1)
    if (first < 0) {
      first = present.nextSetBit(0)
    }
    present.clear(first)
    first
  }

  def contains(e: Int) = present.get(e)

  def nonEmpty = !present.isEmpty()

  def enqueueAll(init: Iterable[Int]): Unit = {
    for (c <- init) {
      present.set(c)
    }
  }

  def remove(e: Int*): Unit = removeAll(e)

  def removeAll(e: Iterable[Int]): Unit = {
    for (c <- e) {
      present.clear(c)
    }
  }

}