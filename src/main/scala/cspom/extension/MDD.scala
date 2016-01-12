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

final case class ShallowEq[A](val m: MDD[A]) {
  override def hashCode = m.hashCode

  override def equals(o: Any) = {
    val ShallowEq(m1) = o
    (m1 eq m) || m1.isInstanceOf[MDDNode[_]] && m.isInstanceOf[MDDNode[_]] && {
      val n = m1.asInstanceOf[MDDNode[A]]
      val m2 = m.asInstanceOf[MDDNode[A]]
      m2.trie.size == n.trie.size && n.trie.forall {
        case (k1, v1) => m2.trie.get(k1).exists(v1 eq _)
      }
    }
  }
}

final case class IdEq[A <: AnyRef](val m: A) {
  override def hashCode = m.hashCode

  override def equals(o: Any) = {
    val IdEq(m1) = o
    m1 eq m
  }
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

  def reduce(): MDD[A] = {

    val cache = new HashMap[Map[A, Int], MDD[A]]()
    val id = new IdMap[MDD[A], Int]()

    id(MDD.leaf) = 0
    var i = 1

    def step1(node: MDD[A]): Unit = {
      if (node eq MDDLeaf) {
        ()
      } else {
        val n = node.asInstanceOf[MDDNode[A]]
        for ((_, c) <- n.trie) step1(c)

        val idc = n.trie
          .map { case (i, c) => i -> id(c) }
          .toMap

        cache.get(idc) match {
          case Some(m) => id(n) = id(m)
          case None    => id(n) = i; i += 1
        }

        cache.update(idc, n)
      }
    }

    step1(this)

    val common = new Array[MDD[A]](i + 1)
    common(0) = MDD.leaf

    def step2(node: MDD[A]): MDD[A] = {

      val idn = id(node)
      if (common(idn) == null) {
        val n = node.asInstanceOf[MDDNode[A]]
        common(idn) = new MDDNode(n.trie.map { case (k, v) => k -> step2(v) }) //new MDDLinkNode(nt.index, step2(nt.child), step2(nt.sibling))
      }
      common(idn)

    }

    step2(this)

  }
  //  /*
  //   * Do not use IdMap here as reduceable MDDs are equal but not ident
  //   */
  //  final def reduce: MDD[A] = reduce(new HashMap[(Int, MDD[A]), MDD[A]](), 0)
  //
  //  def reduce(mdds: mutable.Map[(Int, MDD[A]), MDD[A]], depth: Int): MDD[A]

  override def toString = s"MDD with $edges edges representing $lambda tuples"

  final def filter(f: (Int, A) => Boolean): MDD[A] = filter(f, 0, new IdMap[MDD[A], MDD[A]]())
  def filter(f: (Int, A) => Boolean, k: Int, mdds: IdMap[MDD[A], MDD[A]]): MDD[A]

  final def project(c: Seq[Int]): MDD[A] = project(c.toSet, 0, new IdMap[MDD[A], MDD[A]]())
  def project(c: Set[Int], k: Int, mdds: IdMap[MDD[A], MDD[A]]): MDD[A]

  override def size = {
    val l = lambda
    if (!l.isValidInt) {
      throw new IndexOutOfBoundsException("Size out of bounds: " + l)
    }
    l.toInt
  }

  final def union(m: MDD[A]): MDD[A] = union(m, new IdMap()).reduce
  def union(m: MDD[A], mdds: IdMap[(MDD[A], MDD[A]), MDD[A]]): MDD[A]

  override final def equals(m: Any) = m match {
    case m: MDD[A] => equals(m, new IdMap())
    case o         => o equals this
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
  def reduce(mdds: mutable.Map[(Int, MDD[Any]), MDD[Any]], depth: Int) = this
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

//  def reduce(mdds: mutable.Map[(Int, MDD[A]), MDD[A]], depth: Int): MDD[A] = {
//    //println(this.toSet)
//    mdds.getOrElseUpdate((depth, this), {
//
//      val reduced = trie.map(e => e._1 -> e._2.reduce(mdds, depth + 1))
//
//      if (same(trie, reduced)) {
//        this
//      } else {
//        new MDDNode(reduced)
//      }
//
//    })
//    //println("cached: " + (reduced eq cached))
//
//  }

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

