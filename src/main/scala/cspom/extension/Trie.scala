package cspom.extension

import scala.annotation.tailrec
import scala.collection.immutable.IntMap
import scala.collection.mutable.HashMap
import cspom.Loggable

object Trie {

  val empty = new Trie(Map.empty, 0)

  val leaf = new Trie(Map.empty, 1)

  def apply(tuples: Array[Int]*) = {
    if (tuples.isEmpty) empty
    else {
      val size = tuples.head.length
      val trie = tuples.foldLeft(empty)(_ + _)
      trie
    }
  }
}

final class Trie(val trie: Map[Int, Trie], override val size: Int)
  extends Set[Array[Int]] with Loggable {
  def get(k: Int) = trie.get(k)

  def depth: Int = {
    if (this eq Trie.leaf) 0
    else {
      1 + trie.values.map(_.depth).max
    }
  }

  override def isEmpty = trie.isEmpty

  def +(t: Int*): Trie = this + t.toArray

  def +(t: Array[Int]): Trie = if (contains(t)) this else this + (t, 0)

  private def +(tuple: Array[Int], i: Int): Trie = {
    if (i >= tuple.length) Trie.leaf
    else {
      val v = tuple(i)
      new Trie(trie + (v -> (trie.getOrElse(v, Trie.empty) + (tuple, i + 1))), size + 1)
    }
  }

  def -(t: Int*): Trie = this - t.toArray

  def -(tuple: Array[Int]): Trie = this - (tuple, 0)

  private def -(tuple: Array[Int], i: Int): Trie = {
    if (i >= tuple.length) {
      if (this eq Trie.leaf) Trie.empty
      else this
    } else get(tuple(i)) match {
      case None => this
      case Some(t) => {
        val newTrie = t - (tuple, i + 1)
        if (newTrie eq Trie.empty) {
          val t = trie - tuple(i)
          if (t.isEmpty) Trie.empty
          else new Trie(trie - tuple(i), size - 1)
        } else if (newTrie.size < t.size) new Trie(trie + (tuple(i) -> newTrie), size - 1)
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

  override lazy val hashCode = trie.hashCode

  override def toString = nodes + " nodes representing " + size + " tuples" // + toString(0)

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
    if (this eq Trie.leaf) this
    else {
      // Warning : filterKeys and mapValues are done lazily !
      val m = trie.filterKeys(f(depth, _)).map {
        case (k, v) =>
          k -> v.filterTrie(f, depth + 1)
      }

      val newSize = m.values.iterator.map(_.size).sum

      if (size == newSize) {
        //logger.info("Same trie : " + this)
        this
      } else {
        val n = m.filter { case (_, v) => v ne Trie.empty }
        if (n.isEmpty) Trie.empty
        else new Trie(n, newSize)
      }
    }
  }

  //  private def asStream: Stream[List[Int]] =
  //    trie.toStream flatMap {
  //      case (i, t) => if (t.isEmpty) Stream(List(i)) else t.asStream map (i :: _)
  //    }

  private def listiterator: Iterator[List[Int]] = trie.iterator flatMap {
    case (i, t) => if (t.isEmpty) Iterator(List(i)) else t.listiterator map (i :: _)
  }

  def iterator = listiterator map (_.toArray)
  //  
  //  private def asIterable: Iterable[List[Int]] = trie flatMap {
  //    case (i, t) => if (t.isEmpty) List(List(i)) else t.asIterable map (i :: _)
  //  } 
  //  
  //  override def toList = asList map (_.toArray)

  def nodes: Int = 1 + trie.values.map(_.nodes).sum

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
  def permute(newOrder: Seq[Int]) = Trie(toList map { t => newOrder.map(t(_)).toArray }: _*)

  def tupleString = iterator map { _.mkString(" ") } mkString "|"

}