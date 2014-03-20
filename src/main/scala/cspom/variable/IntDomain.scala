package cspom.variable

import scala.collection.immutable.SortedSet

sealed trait IntDomain extends SortedSet[Int] {
  def intersect(domain: IntDomain): IntDomain
}

object IntDomain {
  def apply(values: Seq[Int]) = {
    //require(values.take(2).size > 1, "constants not accepted, use appropriate constructor")
    values match {
      case r: Range if r.step == 1 => new IntInterval(r.head, r.last)
      case s: Seq[Int] if (values.last - values.head == values.size - 1) => new IntInterval(s.head, s.last)
      case s => IntSeq(SortedSet(values: _*))
    }
  }

}

final case class IntSeq(val values: SortedSet[Int]) extends IntDomain {
  override def toString =
    if (size > 5) {
      values.take(5).mkString("{", ", ", "...}")
    } else {
      values.mkString("{", ", ", "}")
    }

  def intersect(domain: IntDomain) = domain match {
    case FreeInt => this
    case _ => new IntSeq(this.values.intersect(domain))
  }

  def iterator: Iterator[Int] = values.iterator

  def keysIteratorFrom(start: Int): Iterator[Int] = values.keysIteratorFrom(start)

  def -(elem: Int): SortedSet[Int] = IntSeq(values - elem)
  def +(elem: Int): SortedSet[Int] = IntSeq(values + elem)
  def contains(elem: Int): Boolean = values(elem)
  implicit def ordering: Ordering[Int] = values.ordering
  def rangeImpl(from: Option[Int], until: Option[Int]): SortedSet[Int] = IntSeq(values.rangeImpl(from, until))
}

final case class IntInterval(lb: Int, ub: Int) extends IntDomain {

  val range = lb to ub

  require(nonEmpty, "lb <= ub required");

  def intersect(domain: IntDomain): IntDomain = domain match {
    case FreeInt => this
    case m: IntInterval =>
      new IntInterval(math.max(head, m.head), math.min(last, m.last))
    case d => d.intersect(this)
  }

  def -(elem: Int): SortedSet[Int] = throw new UnsupportedOperationException
  def +(elem: Int): SortedSet[Int] = IntDomain(elem +: range)
  def contains(elem: Int): Boolean = range.contains(elem)
  implicit def ordering: Ordering[Int] = throw new UnsupportedOperationException
  def rangeImpl(from: Option[Int], until: Option[Int]): SortedSet[Int] = throw new UnsupportedOperationException
  def iterator: Iterator[Int] = range.iterator
  def keysIteratorFrom(start: Int): Iterator[Int] = ???
  override def toString = s"[$lb..$ub]"
}

case object FreeInt extends IntDomain {
  def -(elem: Int): SortedSet[Int] = throw new UnsupportedOperationException
  def +(elem: Int): SortedSet[Int] = throw new UnsupportedOperationException
  def contains(elem: Int): Boolean = elem.isInstanceOf[Int]
  implicit def ordering: Ordering[Int] = throw new UnsupportedOperationException
  def rangeImpl(from: Option[Int], until: Option[Int]): SortedSet[Int] = throw new UnsupportedOperationException

  def intersect(domain: IntDomain) = domain
  def iterator: Iterator[Int] = throw new UnsupportedOperationException
  def keysIteratorFrom(start: Int): Iterator[Int] = throw new UnsupportedOperationException

  override def equals(o: Any) = o match {
    case ar: AnyRef => FreeInt eq ar
    case _ => false
  }
  override def toString = "?"
}
