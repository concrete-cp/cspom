package cspom.extension

import scala.collection.mutable
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import com.typesafe.scalalogging.slf4j.LazyLogging

object MDD {
  def leaf[A] = MDDLeaf.asInstanceOf[MDD[A]]

  val _empty = MDDNode[Nothing](Map())

  def empty[A] = _empty.asInstanceOf[MDD[A]]
  def apply[A](t: Iterable[Seq[A]]): MDD[A] = t.foldLeft[MDD[A]](empty)(_ + (_: _*))

}

sealed trait MDD[A] extends Relation[A] {

  def +(t: A*): MDD[A]

  def iterator: Iterator[List[A]]

  final def edges: Int = edges(mutable.Set[MDD[A]]())
  def edges(es: mutable.Set[MDD[A]]): Int

  final def lambda: BigInt = lambda(mutable.Map[MDD[A], BigInt]())
  def lambda(ls: mutable.Map[MDD[A], BigInt]): BigInt

  final def reduce: MDD[A] = reduce(mutable.Map[Map[A, MDD[A]], MDD[A]]())
  def reduce(mdds: mutable.Map[Map[A, MDD[A]], MDD[A]]): MDD[A]

  override def toString = s"MDD with $edges edges representing $lambda tuples"

  final def filter(f: (Int, A) => Boolean): MDD[A] = filter(f, 0, mutable.Map[MDD[A], MDD[A]]()).reduce
  def filter(f: (Int, A) => Boolean, k: Int, mdds: mutable.Map[MDD[A], MDD[A]]): MDD[A]

  final def project(c: Seq[Int]): MDD[A] = project(c.toSet, 0, mutable.Map[MDD[A], MDD[A]]()).reduce
  def project(c: Set[Int], k: Int, mdds: mutable.Map[MDD[A], MDD[A]]): MDD[A]

  override def size = lambda.toInt

  final def union(m: MDD[A]): MDD[A] = union(m, mutable.Map())

  def union(m: MDD[A], mdds: mutable.Map[(MDD[A], MDD[A]), MDD[A]]): MDD[A]
}

case object MDDLeaf extends MDD[Any] {
  override def isEmpty = false
  def +(t: Any*) = {
    require(t.isEmpty)
    this
  }
  def iterator = Iterator(Nil)
  def arity = 0
  def contains(t: Seq[Any]) = {
    require(t.isEmpty)
    true
  }
  def edges(e: mutable.Set[MDD[Any]]) = 0
  def lambda(ls: mutable.Map[MDD[Any], BigInt]) = BigInt(1)
  def reduce(mdds: mutable.Map[Map[Any, MDD[Any]], MDD[Any]]) = this
  def filter(f: (Int, Any) => Boolean, k: Int, mdds: mutable.Map[MDD[Any], MDD[Any]]) = this
  def project(c: Set[Int], k: Int, mdds: mutable.Map[MDD[Any], MDD[Any]]) = this
  def union(m: MDD[Any], mdds: mutable.Map[(MDD[Any], MDD[Any]), MDD[Any]]) = this
}

final case class MDDNode[A](val trie: Map[A, MDD[A]]) extends MDD[A] with LazyLogging {
  override def isEmpty = false
  def +(t: A*) = {
    if (t.isEmpty) { MDD.leaf }
    else {
      val v = t.head
      val newTrie = trie.updated(v, trie.getOrElse(v, MDD.empty) + (t.tail: _*))
      new MDDNode(newTrie)
    }

  }
  def iterator = trie.iterator.flatMap { case (k, t) => t.iterator.map(k :: _) }
  def arity = trie.headOption.map(_._2.arity + 1).getOrElse(0)
  def contains(t: Seq[A]) = {
    trie.get(t.head).exists(_.contains(t.tail))
  }
  def edges(e: mutable.Set[MDD[A]]) = {
    if (e.contains(this)) {
      0
    } else {
      e.add(this)
      trie.size + trie.values.iterator.map(t => t.edges(e)).sum
    }
  }

  def lambda(ls: mutable.Map[MDD[A], BigInt]): BigInt = {
    ls.getOrElseUpdate(this, trie.values.iterator.map(_.lambda(ls)).sum)
    //trie.values.map(m => ls.getOrElseUpdate(m, m.lambda(ls))).sum
  }

  override val hashCode: Int = trie.hashCode

  override def equals(o: Any): Boolean = o match {
    case t: MDDNode[A] =>
      trie.size == t.trie.size && trie.forall {
        case (k1, v1) => t.trie.get(k1).exists(_ eq v1)
      }
    case _ => false
  }

  def reduce(mdds: mutable.Map[Map[A, MDD[A]], MDD[A]]): MDD[A] = {
    mdds.getOrElseUpdate(trie,
      new MDDNode(trie.map(e => e._1 -> e._2.reduce(mdds))))
  }

  def filter(f: (Int, A) => Boolean, k: Int, mdds: mutable.Map[MDD[A], MDD[A]]): MDD[A] = mdds.getOrElseUpdate(this, {
    val newTrie: Map[A, MDD[A]] = trie.filter(e => f(k, e._1)).map(e => e._1 -> e._2.filter(f, k + 1, mdds))
    if (newTrie.isEmpty) {
      MDD.empty
    } else {
      new MDDNode(newTrie)
    }
  })

  def union(m: MDD[A], mdds: mutable.Map[(MDD[A], MDD[A]), MDD[A]]) =
    mdds.getOrElseUpdate((this, m), m match {
      case l if l eq MDDLeaf =>
        logger.warn("Union with shorter MDD"); l
      case MDDNode(t2) =>
        new MDDNode(trie ++ t2 map {
          case (k, m) => k -> trie.get(k).map(_ union m).getOrElse(m)
        })

    })

  def project(c: Set[Int], k: Int, mdds: mutable.Map[MDD[A], MDD[A]]): MDD[A] =
    mdds.getOrElseUpdate(this, {
      if (c(k)) {
        new MDDNode(trie.map(e => e._1 -> e._2.project(c, k + 1, mdds)))
      } else {
        val t = trie.values.map(_.project(c, k + 1, mdds))
        if (t.isEmpty) {
          MDD.empty
        } else {
          t.reduceLeft(_ union _)
        }
      }
    })
}

