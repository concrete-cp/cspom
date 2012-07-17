package cspom.extension

import scala.annotation.tailrec
import scala.collection.immutable.IntMap
import scala.collection.mutable.HashMap

object Trie {
  var cache = new HashMap[Int, Trie]()

  def empty(depth: Int) = cache.getOrElseUpdate(depth, new Trie(IntMap.empty, 0, depth))

  def apply(tuples: Array[Int]*) = {
    if (tuples.isEmpty) empty(0)
    else {
      val size = tuples.head.length
      tuples.foldLeft(empty(size))(_ + _)
    }
  }
}

final class Trie(val trie: IntMap[Trie], override val size: Int, val maxDepth: Int) extends Set[Array[Int]] {

  def arity = maxDepth

  def get(k: Int) = trie.get(k)

  override def isEmpty = trie.isEmpty

  def +(t: Int*): Trie = this + t.toArray

  def +(t: Array[Int]): Trie = if (contains(t)) this else this + (t, 0)

  private def +(tuple: Array[Int], i: Int): Trie = {
    assert(tuple.length - i == maxDepth)
    if (i >= tuple.length) this
    else new Trie(trie + (tuple(i) -> (trie.getOrElse(tuple(i), Trie.empty(maxDepth - 1)) + (tuple, i + 1))), size + 1, maxDepth)
  }

  def -(tuple: Array[Int]): Trie = this - (tuple, 0)

  private def -(tuple: Array[Int], i: Int): Trie = {
    if (i >= tuple.length) this
    else get(tuple(i)) match {
      case None => this
      case Some(t) => {
        val newTrie = t - (tuple, i + 1)
        if (newTrie.isEmpty) new Trie(trie - tuple(i), size - 1, maxDepth)
        else if (newTrie.size < t.size) new Trie(trie + (tuple(i) -> newTrie), size - 1, maxDepth)
        else this
      }
    }

  }

  def contains(t: Int*): Boolean = contains(t.toArray)

  def contains(tuple: Array[Int]) = contains(tuple, 0)

  @tailrec
  private def contains(tuple: Array[Int], i: Int): Boolean = {
    if (i >= tuple.size) true
    else get(tuple(i)) match {
      case None => false
      case Some(t) => t.contains(tuple, i + 1)
    }
  }

  override def equals(o: Any): Boolean = o match {
    case t: Trie => trie == t.trie
    case _ => false
  }

  override def toString = size + " elements\n" + toString(0)

  private def toString(depth: Int): String =
    trie.map {
      case (k: Int, v: Trie) =>
        List.fill(depth)(" ").mkString + k + "\n" + v.toString(depth + 1)
    }.mkString

  def foreachTrie(f: (Int, Int) => Unit, depth: Int = 0) {
    for ((k, v) <- trie) {
      f(depth, k)
      v.foreachTrie(f, depth + 1)
    }
  }

  def filterTrie(f: (Int, Int) => Boolean, depth: Int = 0): Trie = {
    if (isEmpty) this
    else {
      val m: IntMap[Trie] = trie.filter {
        case (k, _) =>
          f(depth, k)
      } map {
        case (k, v) =>
          k -> v.filterTrie(f, depth + 1)
      }

      val n: IntMap[Trie] =
        if (depth < maxDepth) {
          m.filter { case (_, v: Trie) => !v.isEmpty }
        } else m

      new Trie(n, n.foldLeft(0)((acc, e) => acc + math.max(1, e._2.size)), maxDepth)
    }
  }

  private def asStream: Stream[List[Int]] =
    trie.toStream flatMap {
      case (i, t) => if (t.isEmpty) Stream(List(i)) else t.asStream map (i :: _)
    }

  def iterator = asStream.iterator.map(_.toArray)

  /**
   * This method returns a copy of this extension with permuted tuples. New
   * order of tuples is given as an argument.
   *
   * <p>
   * For example, a ternary extension 1 2 3|1 3 4|2 4 5 will be reversed to 1
   * 3 2|1 4 3|2 5 4 by a call to reverse(0, 2, 1).
   * </p>
   *
   * @param newOrder
   *            new order of the extension.
   * @return a reversed copy of the extension.
   */
  def permute(newOrder: Seq[Int]) = Trie(asStream map { t => newOrder.map(t(_)).toArray }: _*)

  def tupleString = asStream map { _.mkString(" ") } mkString "|"

}