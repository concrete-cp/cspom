package cspom.variable

import scala.collection.immutable.List

class IntInterval(lb: Int, ub: Int) extends Range.Inclusive(lb, ub, 1) with IntDomain {
  require(lb <= ub, "lb <= ub required");

  def intersect[Int](domain: IntDomain): IntDomain = domain match {
    case m: IntInterval =>
      new IntInterval(math.max(head, m.head), math.min(last, m.last))
    case d => d.intersect(this)
  }

  def contains(value: Any) = value match {
    case v: Int => lb <= v && v <= ub
    case _ => false
  }

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

  def valueOf(interval: String) = {
    val fromto = interval.trim().split("\\.\\.");
    if (fromto.length != 2) {
      throw new NumberFormatException("Interval format must be a..b");
    }
    val lb = Integer.parseInt(fromto(0));
    val ub = Integer.parseInt(fromto(1));
    new IntInterval(lb, ub);
  }

}