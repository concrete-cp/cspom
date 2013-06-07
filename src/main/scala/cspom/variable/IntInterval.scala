package cspom.variable

import scala.collection.immutable.List
import scala.math.{ max, min }
class IntInterval(val lb: Int, val ub: Int) extends CSPOMDomain[Int] {
  require(lb <= ub, "lb <= ub required");

  val values = (lb to ub)

  def intersect[Int](domain: CSPOMDomain[Int]): CSPOMDomain[Int] = domain match {
    case m: IntInterval =>
      new IntInterval(max(lb, m.lb), min(ub, m.ub)).asInstanceOf[CSPOMDomain[Int]];
    case _ => domain.intersect(this.asInstanceOf[CSPOMDomain[Int]])
  }

  def contains(value: Any) = value match {
    case v: Int => lb <= v && v <= ub
    case _ => false
  }

  override def toString = "[" + toXCSP + "]"
  override val hashCode = 31 * lb + ub;
  override val size = 1 + ub - lb

  override def equals(obj: Any) = obj match {
    case i: IntInterval => lb == i.lb && ub == i.ub
    case d: CSPOMDomain[_] => values == d.values
    case _ => false
  }

  def toXCSP = lb + ".." + ub

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