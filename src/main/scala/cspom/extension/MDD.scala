package cspom.extension

import scala.collection.mutable
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import com.typesafe.scalalogging.LazyLogging
import java.util.IdentityHashMap
import scala.collection.JavaConversions

object MDD {
  def leaf[A] = MDDLeaf.asInstanceOf[MDD[A]]

  val _empty = MDDNode[Nothing](Map())

  def empty[A] = _empty.asInstanceOf[MDD[A]]
  def apply[A](t: Iterable[Seq[A]]): MDD[A] = t.foldLeft[MDD[A]](empty)(_ + _)

}

final class IdMap[A, B] extends mutable.Map[A, B] {
  val idMap = new IdentityHashMap[A, B]

  def +=(kv: (A, B)) = {
    idMap.put(kv._1, kv._2)
    this
  }

  def -=(key: A) = {
    idMap.remove(key)
    this
  }

  def get(key: A) = Option(idMap.get(key))

  def iterator = JavaConversions.mapAsScalaMap(idMap).iterator

}

final class IdSet[A] extends mutable.Set[A] {
  val idMap = new IdentityHashMap[A, Unit]
  def iterator: Iterator[A] = JavaConversions.asScalaSet(idMap.keySet).iterator

  def -=(elem: A) = {
    idMap.remove(elem)
    this
  }

  def +=(elem: A) = {
    idMap.put(elem, Unit)
    this
  }
  def contains(elem: A): Boolean = idMap.containsKey(elem)
}

sealed trait MDD[A] extends Relation[A] {

  def +(t: Seq[A]): MDD[A]

  def -(t: Seq[A]) = ???

  def iterator: Iterator[List[A]]

  final def edges: Int = edges(new IdSet[MDD[A]]())
  def edges(es: IdSet[MDD[A]]): Int

  final def lambda: BigInt = lambda(new IdMap[MDD[A], BigInt]())
  def lambda(ls: IdMap[MDD[A], BigInt]): BigInt

  /*
   * Do not use IdMap here as reduceable MDDs are equal but not ident
   */
  final def reduce = reduce2

  final def reduce1: MDD[A] = reduce1(new HashMap())
  def reduce1(mdds: mutable.Map[Map[A, MDD[A]], MDD[A]]): MDD[A]

  final def reduce2: MDD[A] = reduce2(new HashMap())
  def reduce2(mdds: mutable.Map[MDD[A], MDD[A]]): MDD[A]

  override def toString = s"MDD with $edges edges representing $lambda tuples"

  final def filter(f: (Int, A) => Boolean): MDD[A] = filter(f, 0, new IdMap[MDD[A], MDD[A]]()).reduce
  def filter(f: (Int, A) => Boolean, k: Int, mdds: IdMap[MDD[A], MDD[A]]): MDD[A]

  final def project(c: Seq[Int]): MDD[A] = project(c.toSet, 0, new IdMap[MDD[A], MDD[A]]()).reduce
  def project(c: Set[Int], k: Int, mdds: IdMap[MDD[A], MDD[A]]): MDD[A]

  override def size = {
    require(lambda.isValidInt)
    lambda.toInt
  }

  final def union(m: MDD[A]): MDD[A] = union(m, new IdMap()).reduce
  def union(m: MDD[A], mdds: IdMap[(MDD[A], MDD[A]), MDD[A]]): MDD[A]

  override final def equals(m: Any) = m match {
    case m: MDD[A] => equals(m, new IdMap())
    case o => o equals this
  }

  def equals(m: MDD[A], mdds: mutable.Map[MDD[A], Boolean]): Boolean

}

case object MDDLeaf extends MDD[Any] {
  override def isEmpty = false
  def +(t: Seq[Any]) = {
    require(t.isEmpty)
    this
  }
  def iterator = Iterator(Nil)
  def contains(t: Seq[Any]) = {
    require(t.isEmpty)
    true
  }
  def edges(e: IdSet[MDD[Any]]) = 0
  def lambda(ls: IdMap[MDD[Any], BigInt]) = BigInt(1)
  def reduce1(mdds: mutable.Map[Map[Any, MDD[Any]], MDD[Any]]) = this
  def reduce2(mdds: mutable.Map[MDD[Any], MDD[Any]]) = this
  def filter(f: (Int, Any) => Boolean, k: Int, mdds: IdMap[MDD[Any], MDD[Any]]) = this
  def project(c: Set[Int], k: Int, mdds: IdMap[MDD[Any], MDD[Any]]) = this
  def union(m: MDD[Any], mdds: IdMap[(MDD[Any], MDD[Any]), MDD[Any]]) = this
  def equals(m: MDD[Any], mdds: mutable.Map[MDD[Any], Boolean]) = {
    m eq this
  }
}

final case class MDDNode[A](val trie: Map[A, MDD[A]]) extends MDD[A] with LazyLogging {
  override def isEmpty = trie.isEmpty
  assert(trie.forall(e => e._2.nonEmpty))

  def +(t: Seq[A]) = {
    if (t.isEmpty) { MDD.leaf }
    else {
      val v = t.head
      val newTrie = trie.updated(v, trie.getOrElse(v, MDD.empty) + t.tail)
      new MDDNode(newTrie)
    }

  }
  def iterator = trie.iterator.flatMap { case (k, t) => t.iterator.map(k :: _) }
  def contains(t: Seq[A]) = {
    trie.get(t.head).exists(_.contains(t.tail))
  }
  def edges(e: IdSet[MDD[A]]) = {
    if (e.contains(this)) {
      0
    } else {
      e.add(this)
      trie.size + trie.values.iterator.map(t => t.edges(e)).sum
    }
  }

  def lambda(ls: IdMap[MDD[A], BigInt]): BigInt = {
    ls.getOrElseUpdate(this, trie.values.iterator.map(_.lambda(ls)).sum)
    //trie.values.map(m => ls.getOrElseUpdate(m, m.lambda(ls))).sum
  }

  override val hashCode: Int = trie.hashCode

  def equals(o: MDD[A], mdds: mutable.Map[MDD[A], Boolean]): Boolean =
    mdds.getOrElseUpdate(o, o match {
      case t: MDDNode[A] =>
        trie.size == t.trie.size && trie.forall {
          case (k1, v1) => t.trie.get(k1).exists(v1.equals(_, mdds))
        }
      case _ => false
    })

  def reduce1(mdds: mutable.Map[Map[A, MDD[A]], MDD[A]]): MDD[A] = {
    mdds.getOrElseUpdate(trie,
      new MDDNode(trie.map(e => e._1 -> e._2.reduce1(mdds))))
  }
  def reduce2(mdds: mutable.Map[MDD[A], MDD[A]]): MDD[A] = {
    mdds.getOrElseUpdate(this,
      new MDDNode(trie.map(e => e._1 -> e._2.reduce2(mdds))))
  }

  def filter(f: (Int, A) => Boolean, k: Int, mdds: IdMap[MDD[A], MDD[A]]): MDD[A] = mdds.getOrElseUpdate(this, {
    val newTrie: Map[A, MDD[A]] = trie
      .filter(e => f(k, e._1))
      .map(e => e._1 -> e._2.filter(f, k + 1, mdds))
      .filter(e => e._2.nonEmpty)

    if (newTrie.isEmpty) {
      MDD.empty
    } else if (same(trie, newTrie)) {
      this
    } else {
      new MDDNode(newTrie)
    }
  })

  private def same(t1: Map[A, MDD[A]], t2: Map[A, MDD[A]]): Boolean =
    t1.hashCode == t2.hashCode && t1.size == t2.size && t1.forall {
      case (k1, v1) => t2.get(k1).exists(v1 eq _)
    }

  def union(m: MDD[A], mdds: IdMap[(MDD[A], MDD[A]), MDD[A]]) =
    mdds.getOrElseUpdate((this, m), m match {
      case l if l eq MDDLeaf =>
        logger.warn("Union with shorter MDD"); l
      case MDDNode(t2) =>
        new MDDNode(trie ++ t2 map {
          case (k, m) => k -> trie.get(k).map(_ union m).getOrElse(m)
        })

    })

  def project(c: Set[Int], k: Int, mdds: IdMap[MDD[A], MDD[A]]): MDD[A] =
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

