package cspom.variable
import scala.collection.JavaConversions

sealed trait IntDomain extends Seq[Int] {
  //def values: Seq[Int]
  def getValues = JavaConversions.asJavaCollection(this)
  def intersect(domain: IntDomain): IntDomain
}

object IntDomain {
  def apply(values: Seq[Int]) = {
    //require(values.take(2).size > 1, "constants not accepted, use appropriate constructor")
    values match {
      case r: Range if r.step == 1 => new IntInterval(r.head, r.last)
      case s: Seq[Int] if (values.last - values.head == values.size - 1) => new IntInterval(s.head, s.last)
      case s => new IntSeq(values)
    }
  }

}

final case class IntSeq(val values: Seq[Int]) extends IntDomain {
  //  override def equals(obj: Any) = obj match {
  //    case ed: IntSeq => ed.values == values
  //    case _ => false
  //  }

  override def toString =
    if (size > 5)
      values.take(5).mkString("{", ", ", "...}")
    else
      values.mkString("{", ", ", "}")

  def toXCSP = values.mkString(", ")

  def intersect(domain: IntDomain) = domain match {
    case FreeInt => this
    case _ => new IntSeq(this.values.intersect(domain))
  }

  def iterator: Iterator[Int] = values.iterator

  def apply(idx: Int): Int = values(idx)
  def length: Int = values.length
}

final case class IntInterval(val lb: Int, val ub: Int) extends Range.Inclusive(lb, ub, 1) with IntDomain {
  require(lb <= ub, "lb <= ub required");

  def intersect(domain: IntDomain): IntDomain = domain match {
    case FreeInt => this
    case m: IntInterval =>
      new IntInterval(math.max(head, m.head), math.min(last, m.last))
    case d => d.intersect(this)
  }

  //  def contains(value: Any) = value match {
  //    case v: Int => lb <= v && v <= ub
  //    case _ => false
  //  }

  override def toString = s"[$head..$last]"
  //override val hashCode = 31 * lb + ub;
  //override val size = 1 + ub - lb

  //  override def equals(obj: Any) = obj match {
  //    case i: IntInterval => lb == i.lb && ub == i.ub
  //    case d: IntDomain => values == d.values
  //    case _ => false
  //  }

}

object IntInterval {
  def valueOf(interval: String) = interval.trim().split("\\.\\.") match {
    case Array(a, b) => new IntInterval(a.toInt, b.toInt)
    case _ => throw new NumberFormatException("Interval format must be a..b");
  }
}

object FreeInt extends IntDomain {
  def intersect(domain: IntDomain) = domain
  def iterator: Iterator[Int] = throw new UnsupportedOperationException
  def apply(idx: Int): Int = throw new UnsupportedOperationException
  def length: Int = throw new UnsupportedOperationException
  def toXCSP = throw new UnsupportedOperationException
  override def equals(o: Any) = o match {
    case ar: AnyRef => FreeInt eq ar
    case _ => false
  }
  override def toString = "?"
}