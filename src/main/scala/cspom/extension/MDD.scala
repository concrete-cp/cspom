package cspom.extension

import scala.collection.mutable.HashMap

object MDD {
  def empty = EmptyMDD
  def apply(t: Seq[Int]*): MDD = t.foldLeft[MDD](empty)(_ + (_: _*))
  var timestamp = 0
}

trait MDD extends Relation {
  def +(t: Array[Int]): MDD = this + (t: _*)
  def +(t: Int*): MDD
  def iterator = lIterator map (_.toArray)
  def lIterator: Iterator[List[Int]]
  def edges: Int = {
    MDD.timestamp += 1
    edges(MDD.timestamp)
  }
  def edges(ts: Int): Int
  def lambda: BigInt
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
  def close {}
  def arity = throw new UnsupportedOperationException
  def contains(t: Seq[Int]) = false
  def edges(ts: Int) = 0
  def lambda = BigInt(0)
  def reduce(mdds: HashMap[Map[Int, MDD], MDD]): MDD = throw new UnsupportedOperationException
}

object MDDLeaf extends MDD {
  override def isEmpty = false
  def +(t: Int*) = throw new UnsupportedOperationException
  def lIterator = Iterator(Nil)
  def close {}
  def arity = 0
  def contains(t: Seq[Int]) = t.isEmpty
  def edges(ts: Int) = 0
  def lambda = BigInt(1)
  def reduce(mdds: HashMap[Map[Int, MDD], MDD]): MDD = this
}

class MDDNode(var trie: Map[Int, MDD]) extends MDD {
  var timestamp: Int = _
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
  def close { trie = null }
  def arity = 1 + trie.head._2.arity
  def contains(t: Seq[Int]) = {
    trie.get(t.head).map(_.contains(t.tail)).getOrElse(false)
  }
  def edges(ts: Int) = {
    if (timestamp == ts) {
      0
    } else {
      timestamp = ts
      trie.size + trie.values.map(_.edges(ts)).sum
    }
  }
  lazy val lambda = trie.values.map(_.lambda).sum

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

