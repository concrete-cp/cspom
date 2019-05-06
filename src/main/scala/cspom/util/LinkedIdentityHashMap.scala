package cspom.util

import scala.collection.mutable

class LinkedIdentityHashMap[A <: AnyRef, B] extends
  mutable.AbstractMap[A, B] with collection.mutable.Map[A, B] with collection.mutable.MapLike[A, B, LinkedIdentityHashMap[A, B]] {

  override def empty = new LinkedIdentityHashMap()

  private val map = new mutable.LinkedHashMap[IdentityWrapper[A], B]()

  override def +=(kv: (A, B)): LinkedIdentityHashMap.this.type = {
    map += new IdentityWrapper(kv._1) -> kv._2
    this
  }

  override def -=(key: A): LinkedIdentityHashMap.this.type = {
    map -= new IdentityWrapper(key)
    this
  }


  override def get(key: A): Option[B] = map.get(new IdentityWrapper(key))

  override def iterator: Iterator[(A, B)] = map.iterator.map { case (idkey, value) => idkey.obj -> value }
}

class IdentityWrapper[T <: AnyRef](val obj: T) {

  override final def equals(o: Any): Boolean = obj eq o.asInstanceOf[IdentityWrapper[T]].obj

  override final def hashCode(): Int = System.identityHashCode(obj)
}
