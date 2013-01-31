package cspom.extension

import scala.annotation.tailrec
import scala.collection.immutable.IntMap
import scala.collection.mutable.HashMap
import cspom.Loggable

object HashTrie {

  val empty = new HashTrie(Map.empty, 0)

  val leaf = new HashTrie(Map.empty, 1)

  def apply(tuples: Array[Int]*) = tuples.foldLeft(empty)(_ + _)
}

final class HashTrie(val trie: Map[Int, HashTrie], override val size: Int)
  extends Relation with Loggable {

  def close() = throw new UnsupportedOperationException

  def arity: Int = {
    if (HashTrie.this eq HashTrie.leaf) 0
    else {
      1 + trie.values.map(_.arity).max
    }
  }

  override def isEmpty = trie.isEmpty

  def +(t: Array[Int]): HashTrie = if (contains(t)) HashTrie.this else HashTrie.this + (t, 0)

  private def +(tuple: Array[Int], i: Int): HashTrie = {
    if (i >= tuple.length) HashTrie.leaf
    else {
      val v = tuple(i)
      new HashTrie(trie + (v -> (trie.getOrElse(v, HashTrie.empty) + (tuple, i + 1))), size + 1)
    }
  }

  def +(t: Int*): HashTrie = if (contains(t.toList)) HashTrie.this else HashTrie.this plus t

  private def plus(t: Seq[Int]): HashTrie = {
    if (t.isEmpty) HashTrie.leaf
    else {
      val v = t.head
      new HashTrie(trie + (v -> (trie.getOrElse(v, HashTrie.empty) plus t.tail)), size + 1)
    }
  }

  def -(t: Int*): HashTrie = HashTrie.this - t.toArray

  def -(tuple: Array[Int]): HashTrie = HashTrie.this - (tuple, 0)

  private def -(tuple: Array[Int], i: Int): HashTrie = {
    if (i >= tuple.length) {
      if (HashTrie.this eq HashTrie.leaf) HashTrie.empty
      else HashTrie.this
    } else trie.get(tuple(i)) match {
      case None => HashTrie.this
      case Some(t) => {
        val newTrie = t - (tuple, i + 1)
        if (newTrie eq HashTrie.empty) {
          val t = trie - tuple(i)
          if (t.isEmpty) HashTrie.empty
          else new HashTrie(trie - tuple(i), size - 1)
        } else if (newTrie.size < t.size) new HashTrie(trie + (tuple(i) -> newTrie), size - 1)
        else HashTrie.this
      }
    }
  }

  def contains(tuple: List[Int]): Boolean = {
    if (tuple.isEmpty) true
    else trie.get(tuple.head) match {
      case None => false
      case Some(t) => t.contains(tuple.tail)
    }
  }

  def contains(tuple: Array[Int]): Boolean = contains(tuple, 0)

  def contains(t: Seq[_]) = contains(t map { i: Any => i.asInstanceOf[Int] } toList)

  @tailrec
  private def contains(tuple: Array[Int], i: Int): Boolean = {
    if (i >= tuple.size) true
    else trie.get(tuple(i)) match {
      case None => false
      case Some(t) => t.contains(tuple, i + 1)
    }
  }

  override def equals(o: Any): Boolean = o match {
    case t: HashTrie => trie == t.trie
    case _ => false
  }

  override lazy val hashCode = trie.hashCode

  override def toString = nodes + " nodes representing " + size + " tuples" // + toString(0)

  private def toString(depth: Int): String =
    trie.map {
      case (k: Int, v: HashTrie) =>
        List.fill(depth)(" ").mkString + k + "\n" + v.toString(depth + 1)
    }.mkString

  def foreachTrie(f: (Int, Int) => Unit, depth: Int = 0) {
    for ((k, v) <- trie) {
      f(depth, k)
      v.foreachTrie(f, depth + 1)
    }
  }

  def filterTrie(f: (Int, Int) => Boolean, depth: Int = 0): HashTrie = {
    if (HashTrie.this eq HashTrie.leaf) HashTrie.this
    else {
      // Warning : filterKeys and mapValues are done lazily !
      val m = trie.filterKeys(f(depth, _)).map {
        case (k, v) =>
          k -> v.filterTrie(f, depth + 1)
      }

      val newSize = m.values.iterator.map(_.size).sum

      if (size == newSize) {
        //logger.info("Same trie : " + this)
        HashTrie.this
      } else {
        val n = m.filter { case (_, v) => v ne HashTrie.empty }
        if (n.isEmpty) HashTrie.empty
        else new HashTrie(n, newSize)
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

}