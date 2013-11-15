package cspom.variable
import scala.collection.JavaConversions

sealed trait IntDomain extends Seq[Int] {
  //def values: Seq[Int]
  def getValues = JavaConversions.asJavaCollection(this)
  def intersect(domain: IntDomain): IntDomain
  def toXCSP: String
}

object IntDomain {
  def of(values: Int*) = {
    //require(values.take(2).size > 1, "constants not accepted, use appropriate constructor")
    values match {
      case r: Range => new IntInterval(r.head, r.last)
      case s: Seq[Int] if (values.last - values.head == values.size - 1) => new IntInterval(s.head, s.last)
      case s => new IntSeq(values)
    }
  }

  /**
   * Parse the given domain given as a String. Domains are usually sequence of
   * values separated by spaces such as "1 3 -4 5" or intervals in the format
   * "a..b". Sequences of values and intervals such as "1 3..10 18..30" are
   * allowed and converted to a sequence of values.
   *
   * @param domain
   *            The String domain to parse
   * @return The resulting Domain object
   */
  def valueOf(desc: String) = {
    val values = desc.trim.split(" +").flatMap { v =>
      if (v.contains("..")) {
        IntInterval.valueOf(v);
      } else {
        Seq(v.trim.toInt);
      }
    }

    of(values: _*)
    //        //        if (values.size == 1) {
    //        //          new Constant(values.head)
    //        //        } else 
    //        if (values == (values.head to values.last)) {
    //          new IntInterval(values.head, values.last)
    //        } else {
    //          new ExtensiveDomain(values)
    //        }

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

  override def toString = "[" + toXCSP + "]"
  //override val hashCode = 31 * lb + ub;
  //override val size = 1 + ub - lb

  //  override def equals(obj: Any) = obj match {
  //    case i: IntInterval => lb == i.lb && ub == i.ub
  //    case d: IntDomain => values == d.values
  //    case _ => false
  //  }

  def toXCSP = head + ".." + last

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