package cspom.compiler

import scala.collection.mutable.Queue
import scala.collection.mutable.HashSet

final class QueueSet[A] {
  private val present = collection.mutable.Set[A]()

  private val queue: Queue[A] = new Queue()

  //def enqueue(e: A*): Unit = e.foreach(enqueue)

  def this(init: Iterable[A]) = {
    this()
    present ++= init
    queue ++= init
  }

  def enqueue(c: A): Unit = {
    if (!present(c)) {
      queue.enqueue(c)
      present += c
    }
  }

  @annotation.tailrec
  def dequeue(): A = {
    val a = queue.dequeue();
    if (present(a)) {
      present -= a
      a
    } else {
      dequeue()
    }

  }

  def nonEmpty = present.nonEmpty

  def enqueueAll(c: Iterable[A]): Unit = {
    c.foreach(enqueue)
  }

  def remove(e: A*): Unit = removeAll(e)

  def removeAll(e: Iterable[A]): Unit = {
    present --= e
  }

}