package cspom.extension

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import MDD.Trie

object MDD {
  type Trie = List[(Int, MDD)]

  def empty = EmptyMDD
  def apply(t: Seq[Int]*): MDD = t.foldLeft[MDD](empty)(_ + (_: _*))
  def fromList(l: Trie) = l.foldLeft[MDD](EmptyMDD) {
    case (acc, (i, m)) => new MDDNode(m, acc, i)
  }
}

trait MDD extends Relation {
  def +(t: Array[Int]): MDD = this + (t: _*)
  def +(t: Int*): MDD
  def iterator = lIterator map (_.toArray)
  def lIterator: Iterator[List[Int]]

  final def edges: Int = edges(new HashSet[Trie]())
  def edges(es: HashSet[Trie]): Int

  final def lambda: BigInt = lambda(new HashMap[MDD, BigInt]())
  def lambda(ls: HashMap[MDD, BigInt]): BigInt

  def reduce: MDD = reduce(new HashMap[List[(Int, MDD)], MDD]())
  def reduce(mdds: HashMap[Trie, MDD]): MDD
  def asList: Trie
  override def toString = s"MDD with $edges edges representing $lambda tuples"
}

object EmptyMDD extends MDD {
  override def isEmpty = true
  def +(t: Int*) =
    if (t.isEmpty) {
      MDDLeaf
    } else {
      new MDDNode(EmptyMDD + (t.tail: _*), EmptyMDD, t.head)
    }
  def lIterator = Iterator()
  def arity = throw new UnsupportedOperationException
  def contains(t: Seq[Int]) = false
  def edges(e: HashSet[Trie]) = 0
  def lambda(ls: HashMap[MDD, BigInt]) = BigInt(0)
  def asList = Nil
  def reduce(mdds: HashMap[Trie, MDD]): MDD = throw new UnsupportedOperationException
}

object MDDLeaf extends MDD {
  override def isEmpty = false
  def +(t: Int*) = throw new UnsupportedOperationException
  def lIterator = Iterator(Nil)
  def arity = 0
  def contains(t: Seq[Int]) = t.isEmpty
  def edges(e: HashSet[Trie]) = 0
  def lambda(ls: HashMap[MDD, BigInt]) = BigInt(1)
  def reduce(mdds: HashMap[Trie, MDD]): MDD = this
  def asList = throw new UnsupportedOperationException

}

final class MDDNode(val child: MDD, val next: MDD, val value: Int) extends MDD {
  override def isEmpty = false
  def +(t: Int*) = {
    if (t.isEmpty) {
      throw new UnsupportedOperationException
    } else {
      val v = t.head
      if (v == value) {
        new MDDNode(child + (t.tail: _*), next, value)
      } else {
        new MDDNode(child, next + (t: _*), value)
      }
    }
  }
  def lIterator = child.lIterator.map(value :: _) ++ next.lIterator
  def arity = 1 + child.arity
  def contains(t: Seq[Int]) = {
    if (value == t.head) {
      child.contains(t.tail)
    } else {
      next.contains(t)
    }
  }
  def edges(e: HashSet[Trie]) = {
    val t = asList
    if (e.contains(t)) {
      0
    } else {
      e += t
      t.size + t.map(_._2.edges(e)).sum
    }
  }

  def lambda(ls: HashMap[MDD, BigInt]): BigInt = {
    ls.getOrElseUpdate(this, {
      child.lambda(ls) + next.lambda(ls)
    })
  }

  override val hashCode: Int = 31 * child.hashCode + next.hashCode

  override def equals(o: Any): Boolean = o match {
    case t: MDDNode => value == t.value && (child eq t.child) && next.equals(t.next)
    case _ => false
  }

  def asList: Trie = {
    (value, child) :: next.asList
  }

  def reduce(mdds: HashMap[Trie, MDD]): MDD = {
    val t = asList
    mdds.getOrElseUpdate(t, this)
  }

}

