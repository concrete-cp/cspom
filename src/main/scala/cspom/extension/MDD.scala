package cspom.extension

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

object MDD {
  def empty = EmptyMDD
  def apply(t: Seq[Int]*): MDD = t.foldLeft[MDD](empty)(_ + (_: _*))

}

trait MDD extends Relation {

  /**
   * For Java interop
   */
  final def add(t: Array[Int]) = this + (t: _*)

  def +(t: Int*): MDD

  def iterator: Iterator[List[Int]]

  final def edges: Int = edges(new HashSet[MDD]())
  def edges(es: HashSet[MDD]): Int

  final def lambda: BigInt = lambda(new HashMap[MDD, BigInt]())
  def lambda(ls: HashMap[MDD, BigInt]): BigInt

  final def reduce: MDD = reduce(new HashMap[Map[Int, MDD], MDD]())
  def reduce(mdds: HashMap[Map[Int, MDD], MDD]): MDD

  override def toString = s"MDD with $edges edges representing $lambda tuples"

  final def filter(f: (Int, Int) => Boolean): MDD = filter(f, 0, new HashMap())
  def filter(f: (Int, Int) => Boolean, k: Int, mdds: HashMap[MDD, MDD]): MDD

  final def project(c: Seq[Int]): MDD = project(c.toSet, 0, new HashMap())
  def project(c: Set[Int], k: Int, mdds: HashMap[MDD, MDD]): MDD

}

object EmptyMDD extends MDD {
  override def isEmpty = true
  def +(t: Int*) =
    if (t.isEmpty) {
      MDDLeaf
    } else {
      new MDDNode(Map(t.head -> (EmptyMDD + (t.tail: _*))))
    }
  def iterator = Iterator.empty
  def arity = throw new UnsupportedOperationException
  def contains(t: Seq[Int]) = false
  def edges(e: HashSet[MDD]) = 0
  def lambda(ls: HashMap[MDD, BigInt]) = BigInt(0)
  def reduce(mdds: HashMap[Map[Int, MDD], MDD]): MDD = throw new UnsupportedOperationException
  def filter(f: (Int, Int) => Boolean, k: Int, mdds: HashMap[MDD, MDD]) = this
  def project(c: Set[Int], k: Int, mdds: HashMap[MDD, MDD]) = this

  def main(args: Array[String]) = {
    val m = MDD(Seq(1, 2, 3), Seq(1, 3, 3), Seq(1, 3, 2), Seq(2, 5, 3)).filter { (k, i) => k != 1 || i != 3 }

    println(m)
    m.foreach(t => println(t.toSeq))

    val m2 = m.project(Seq(0, 2))
    println(m2)
    m2.foreach(t => println(t.toSeq))
  }
}

object MDDLeaf extends MDD {
  override def isEmpty = false
  def +(t: Int*) = {
    require(t.isEmpty)
    this
  }
  def iterator = Iterator(Nil)
  def arity = 0
  def contains(t: Seq[Int]) = t.isEmpty
  def edges(e: HashSet[MDD]) = 0
  def lambda(ls: HashMap[MDD, BigInt]) = BigInt(1)
  def reduce(mdds: HashMap[Map[Int, MDD], MDD]): MDD = this
  def filter(f: (Int, Int) => Boolean, k: Int, mdds: HashMap[MDD, MDD]) = this
  def project(c: Set[Int], k: Int, mdds: HashMap[MDD, MDD]) = this
}

final class MDDNode(val trie: Map[Int, MDD]) extends MDD {
  override def isEmpty = false
  def +(t: Int*) = {
    require(t.nonEmpty)

    val v = t.head
    val newTrie = trie.updated(v, trie.getOrElse(v, EmptyMDD) + (t.tail: _*))
    new MDDNode(newTrie)

  }
  def iterator = trie.iterator.flatMap { case (k, t) => t.iterator.map(k :: _) }
  def arity = 1 + trie.head._2.arity
  def contains(t: Seq[Int]) = {
    trie.get(t.head).map(_.contains(t.tail)).getOrElse(false)
  }
  def edges(e: HashSet[MDD]) = {
    if (e.contains(this)) {
      0
    } else {
      e += this
      trie.size + trie.values.map(t => t.edges(e)).sum
    }
  }

  def lambda(ls: HashMap[MDD, BigInt]): BigInt = {
    ls.getOrElseUpdate(this, trie.values.map(_.lambda(ls)).sum)
    //trie.values.map(m => ls.getOrElseUpdate(m, m.lambda(ls))).sum
  }

  override lazy val hashCode: Int = trie.hashCode

  override def equals(o: Any): Boolean = o match {
    case t: MDDNode =>
      trie.size == t.trie.size && trie.forall {
        case (k1, v1) => t.trie.get(k1).map(_ eq v1).exists(b => b)
      }
    case _ => false
  }

  def reduce(mdds: HashMap[Map[Int, MDD], MDD]): MDD = {
    mdds.getOrElseUpdate(trie,
      new MDDNode(trie.map(e => e._1 -> e._2.reduce(mdds))))
  }

  def filter(f: (Int, Int) => Boolean, k: Int, mdds: HashMap[MDD, MDD]): MDD = mdds.getOrElseUpdate(this, {
    val newTrie: Map[Int, MDD] = trie.filter(e => f(k, e._1)).map(e => e._1 -> e._2.filter(f, k + 1, mdds))
    if (newTrie.isEmpty) {
      EmptyMDD
    } else {
      new MDDNode(newTrie)
    }
  })

  def project(c: Set[Int], k: Int, mdds: HashMap[MDD, MDD]): MDD = mdds.getOrElseUpdate(this, {
    if (c(k)) {
      new MDDNode(trie.map(e => e._1 -> e._2.project(c, k + 1, mdds)))
    } else {
      trie.values.map(_.project(c, k + 1, mdds)).flatten.foldLeft[MDD](EmptyMDD)(_ + (_: _*))
    }
  })
}

