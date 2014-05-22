package cspom.util

import com.google.common.collect.Range

class GuavaRange[A <% Ordered[A]](val r: Range[AsOrdered[A]]) {

}

private[cspom] case class AsOrdered[T <% Ordered[T]](val value: T) extends Ordered[AsOrdered[T]] {
  override def compare(that: AsOrdered[T]) = value.compare(that.value)
  override def toString = value.toString
}

/**
 * Factory for AsOrdered
 */
private[cspom] final object AsOrdered {

  /**
   * Implicit conversion from `Ordering[T]` to `AsOrdered[T]`
   */
  implicit def asOrdered[T](value: T)(implicit ord: Ordering[T]): AsOrdered[T] =
    AsOrdered(value)
}