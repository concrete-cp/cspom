package cspom.util
import scala.reflect.runtime.universe._
import scala.reflect.ClassTag
import scala.collection.mutable.AbstractMap
import scala.collection.mutable.ResizableArray

class VecMap[T >: Null: ClassTag](buckets: Int = 16)
    extends AbstractMap[Int, T] with collection.mutable.Map[Int, T] with collection.mutable.MapLike[Int, T, VecMap[T]] {
  var map: Array[T] = new Array(buckets)

  def get(key: Int): Option[T] =
    if (key < map.length && map(key) != null) Some(map(key))
    else None

  def iterator: Iterator[(Int, T)] =
    map.iterator.zipWithIndex.filter {
      null != _._1
    }
      .map(_.swap)

  def -=(key: Int): this.type = {
    map(key) = null
    this
  }

  def +=(kv: (Int, T)): this.type = {
    val (key, value) = kv
    ensureCapacity(key)
    map(key) = value
    this
  }

  private def ensureCapacity(n: Int): Unit = {
    // Use a Long to prevent overflows
    val arrayLength: Long = map.length
    if (n >= arrayLength) {
      var newSize: Long = arrayLength * 2
      while (n >= newSize)
        newSize = newSize * 2
      // Clamp newSize to Int.MaxValue
      if (newSize > Int.MaxValue) newSize = Int.MaxValue

      val newArray: Array[T] = new Array(newSize.toInt)
      scala.compat.Platform.arraycopy(map, 0, newArray, 0, map.length)
      map = newArray
    }

  }

  override def empty = new VecMap()

}