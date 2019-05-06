package cspom.util
import scala.collection.mutable

class VecMap[T](buckets: Int = 16)
    extends mutable.AbstractMap[Int, T] with collection.mutable.Map[Int, T] with collection.mutable.MapLike[Int, T, VecMap[T]] {
  var map: Array[Option[T]] = Array.fill(buckets)(None)

  def get(key: Int): Option[T] =
    if (key < map.length) {
      map(key)
    } else None

  def iterator: Iterator[(Int, T)] =
    map.iterator.zipWithIndex.collect {
      case (Some(v), i) => i -> v
    }

  def -=(key: Int): this.type = {
    map(key) = None
    this
  }

  def +=(kv: (Int, T)): this.type = {
    update(kv._1, kv._2)
    this
  }

  override def update(key: Int, value: T): Unit = {
    ensureCapacity(key)
    map(key) = Some(value)
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

      map = map.padTo(newSize.toInt, None)
    }

  }

  override def empty = new VecMap()

}