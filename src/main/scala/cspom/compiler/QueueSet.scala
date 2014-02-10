package cspom.compiler

import scala.collection.mutable.Queue
import scala.collection.mutable.HashSet

final class QueueSet[A] {
  private val present = collection.mutable.Set[A]()

  private val queue: Queue[A] = new Queue()

  //def enqueue(e: A*): Unit = e.foreach(enqueue)

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

  def remove(e: A*) {
    present --= e
  }

}