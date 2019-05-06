package cspom.compiler

import java.util

class QueueSet(present: util.BitSet) {

  private var first = -1

  def this() = this(new util.BitSet())

  def iterator: Iterator[Int] = new Iterator[Int] {
    private var i = present.nextSetBit(0)

    def hasNext: Boolean = i >= 0

    def next(): Int = {
      val t = i
      i = present.nextSetBit(i + 1)
      t
    }
  }

  def this(init: Iterable[Int]) = {
    this(if (init.isEmpty) new util.BitSet() else new util.BitSet(init.max + 1))
    enqueueAll(init)
  }

  def enqueue(c: Int): Unit = {
    present.set(c)
  }

  def dequeue(): Int = {
    require(nonEmpty)
    first = present.nextSetBit(first + 1)
    if (first < 0) first = present.nextSetBit(0)
    present.clear(first)
    first
  }

  def contains(e: Int): Boolean = present.get(e)

  def nonEmpty: Boolean = !present.isEmpty

  def isEmpty: Boolean = present.isEmpty

  def enqueueAll(init: Traversable[Int]): Unit = {
    for (i <- init) present.set(i)
  }

  def remove(e: Int*): Unit = removeAll(e)

  def removeAll(e: Iterable[Int]): Unit = {
    for (i <- e) present.clear(i)
  }

}