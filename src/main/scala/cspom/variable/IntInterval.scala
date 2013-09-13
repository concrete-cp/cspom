package cspom.variable

import scala.collection.immutable.List

class IntInterval(val lb: Int, val ub: Int) extends Range.Inclusive(lb, ub, 1) with IntDomain {
  require(lb <= ub, "lb <= ub required");

  def intersect(domain: IntDomain): IntDomain = domain match {
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