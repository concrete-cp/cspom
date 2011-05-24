
package scala.collection
package mutable

import collection.generic._

class MyLinkedHashSet[A] extends Set[A]
  with GenericSetTemplate[A, MyLinkedHashSet]
  with SetLike[A, MyLinkedHashSet[A]] {
  override def companion: GenericCompanion[MyLinkedHashSet] = MyLinkedHashSet

  override def size = hashSet.size

  def contains(elem: A): Boolean = hashSet.contains(new Cell(elem))

  def +=(elem: A): this.type = { add(elem); this }
  def -=(elem: A): this.type = { remove(elem); this }

  private val hashSet = new HashSet[Cell[A]]
  private var firstCell: Cell[A] = null
  private var lastCell: Cell[A] = null

  override def add(elem: A): Boolean = {
    val c = new Cell(elem)
    if (hashSet.add(c)) {
      if (lastCell == null) {
        assert(firstCell == null)
        firstCell = c;
      } else {
        c.prev = lastCell;
        lastCell.next = c;
      }
      lastCell = c
      true
    } else false
  }

  override def remove(elem: A): Boolean = {
    val c = new Cell(elem)
    if (hashSet.remove(c)) {
      if (c.prev == null) {
        firstCell = c.next
      } else {
        c.prev.next = c.next
      }
      if (c.next == null) {
        lastCell = c.prev
      } else {
        c.next.prev = c.prev
      }
      true
    } else false
  }

  override def clear() {
    hashSet.clear()
    firstCell = null
    lastCell = null
  }

  def iterator = new Iterator[A] {
    var current = firstCell
    def hasNext = current != null
    def next = {
      val ret = current;
      current = current.next;
      ret.content;
    }
  }

  override def foreach[U](f: A => U) = iterator foreach f

}

class Cell[A](val content: A) {
  var next: Cell[A] = null;
  var prev: Cell[A] = null;

  override val hashCode = content.hashCode
  override def equals(obj: Any) = obj match {
    case c: Cell[A] => content == c.content
    case _ => false
  }
}

/**
 * $factoryInfo
 *  @define Coll MyLinkedHashSet
 *  @define coll linked hash set
 */
object MyLinkedHashSet extends MutableSetFactory[MyLinkedHashSet] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, MyLinkedHashSet[A]] = setCanBuildFrom[A]
  override def empty[A]: MyLinkedHashSet[A] = new MyLinkedHashSet[A]
}

