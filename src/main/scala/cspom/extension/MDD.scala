package cspom.extension

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

object MDD {
  def empty = EmptyMDD
  def apply(t: Seq[Int]*): MDD = t.foldLeft[MDD](empty)(_ + (_: _*))
}

trait MDD extends Relation {
  def +(t: Array[Int]): MDD = this + (t: _*)
  def +(t: Int*): MDD
  def iterator = lIterator map (_.toArray)
  def lIterator: Iterator[List[Int]]

  final def edges: Int = edges(new HashSet[MDD]())
  def edges(es: HashSet[MDD]): Int

  final def lambda: BigInt = lambda(new HashMap[MDD, BigInt]())
  def lambda(ls: HashMap[MDD, BigInt]): BigInt

  def reduce: MDD = reduce(new HashMap[Map[Int, MDD], MDD]())
  def reduce(mdds: HashMap[Map[Int, MDD], MDD]): MDD
  override def toString = s"MDD with $edges edges representing $lambda tuples"
}

object EmptyMDD extends MDD {
  override def isEmpty = true
  def +(t: Int*) =
    if (t.isEmpty) {
      MDDLeaf
    } else {
      new MDDNode(Map(t.head -> (EmptyMDD + (t.tail: _*))))
    }
  def lIterator = Iterator()
  def arity = throw new UnsupportedOperationException
  def contains(t: Seq[Int]) = false
  def edges(e: HashSet[MDD]) = 0
  def lambda(ls: HashMap[MDD, BigInt]) = BigInt(0)
  def reduce(mdds: HashMap[Map[Int, MDD], MDD]): MDD = throw new UnsupportedOperationException
}

object MDDLeaf extends MDD {
  override def isEmpty = false
  def +(t: Int*) = throw new UnsupportedOperationException
  def lIterator = Iterator(Nil)
  def arity = 0
  def contains(t: Seq[Int]) = t.isEmpty
  def edges(e: HashSet[MDD]) = 0
  def lambda(ls: HashMap[MDD, BigInt]) = BigInt(1)
  def reduce(mdds: HashMap[Map[Int, MDD], MDD]): MDD = this
}

final class MDDNode(val trie: Map[Int, MDD]) extends MDD {
  override def isEmpty = false
  def +(t: Int*) = {
    if (t.isEmpty) {
      throw new UnsupportedOperationException
    } else {
      val v = t.head
      val newTrie = trie.updated(t.head, trie.getOrElse(t.head, EmptyMDD) + (t.tail: _*))
      new MDDNode(newTrie)
    }
  }
  def lIterator = trie.iterator.flatMap { case (k, t) => t.lIterator.map(k :: _) }
  def arity = 1 + trie.head._2.arity
  def contains(t: Seq[Int]) = {
    trie.get(t.head).map(_.contains(t.tail)).getOrElse(false)
  }
  def edges(e: HashSet[MDD]) = {
    if (e.contains(this)) {
      0
    } else {
      e += this
      trie.size + trie.values.map(_.edges(e)).sum
    }
  }

  def lambda(ls: HashMap[MDD, BigInt]): BigInt = {
    trie.values.map(m => ls.getOrElseUpdate(m, lambda(ls))).sum
  }

  override lazy val hashCode: Int = trie.hashCode

  override def equals(o: Any): Boolean = o match {
    case t: MDDNode =>
      trie.size == t.trie.size && trie.forall {
        case (k1, v1) => t.trie.get(k1).map(_ eq v1).getOrElse(false)
      }
    case _ => false
  }

  def reduce(mdds: HashMap[Map[Int, MDD], MDD]): MDD = {
    var b = trie.map(e => e._1 -> e._2.reduce(mdds))
    mdds.getOrElseUpdate(b, new MDDNode(b))
  }

}

