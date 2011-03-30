package cspom
import java.util.{ AbstractList, Iterator }

case class ListWrapper[T](val list: List[T]) extends AbstractList[T] {
  def get(i: Int) = list(i)
  def size: Int = list.size

  // the default AbstractList version of iterator gets the size and then calls
  // get(0), get(1), etc. But that doesn’t work so well with a linked list. The
  // better way to do an iterator on a linked list is to keep track of the
  // portion of the list that hasn’t been iterated over
  override def iterator: Iterator[T] =
    new Iterator[T] {
      private var remainder = list

      def remove = throw new UnsupportedOperationException("can’t remove from an immutable list")
      def next: T = remainder match {
        case head :: tail => {
          remainder = tail
          head
        }
        case _ => throw new IndexOutOfBoundsException("tail of an empty list")
      }
      def hasNext = !remainder.isEmpty
    }

  // case classes get a hashCode and equals, but we need to follow the java.util.List standard -
  // the case class generated versions would be too strict. We’ll just use the versions defined
  // in AbstractList
  override def hashCode = super.hashCode
  override def equals(other: Any) = super.equals(other)
}