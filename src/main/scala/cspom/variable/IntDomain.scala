package cspom.variable

import scala.collection.SortedSet

sealed trait IntDomain extends SortedSet[Int] {
  def intersect(domain: IntDomain): IntDomain
}

object IntDomain {
  def apply(values: SortedSet[Int]) = {
    //require(values.take(2).size > 1, "constants not accepted, use appropriate constructor")
    values match {
      case r: Range if r.step == 1 => new IntInterval(r.head, r.last)
      case s: Seq[Int] if (values.last - values.head == values.size - 1) => new IntInterval(s.head, s.last)
      case s => new IntSeq(values)
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

  def apply(idx: Int): Int = values(idx)
  def length: Int = values.length
}

final case class IntInterval(_lb: Int, _ub: Int) extends Range.Inclusive(_lb, _ub, 1) with IntDomain {
  require(nonEmpty, "lb <= ub required");

  def lb = head

  def ub = last

  def intersect(domain: IntDomain): IntDomain = domain match {
    case FreeInt => this
    case m: IntInterval =>
      new IntInterval(math.max(head, m.head), math.min(last, m.last))
    case d => d.intersect(this)
  }

  override def toString = s"[$head..$last]"
}

case object FreeInt extends IntDomain {
  def intersect(domain: IntDomain) = domain
  def iterator: Iterator[Int] = throw new UnsupportedOperationException
  def apply(idx: Int): Int = throw new UnsupportedOperationException
  def length: Int = throw new UnsupportedOperationException

  override def equals(o: Any) = o match {
    case ar: AnyRef => FreeInt eq ar
    case _ => false
  }
  override def toString = "?"
  override def contains(o: Any) = o.isInstanceOf[Int]
}
