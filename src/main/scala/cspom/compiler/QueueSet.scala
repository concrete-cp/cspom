package cspom.compiler

import scala.collection.mutable.Queue
import scala.collection.mutable.HashSet

class QueueSet[A] {
  /**
   *
   */
  val serialVersionUID = 1L;

  private val present = collection.mutable.Set[A]()

  private val queue: Queue[A] = new Queue()

  def enqueue(e: A*): Unit = {
    for (c <- e if (!present(c))) {
      queue.enqueue(c)
      present += c
    }
  }

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