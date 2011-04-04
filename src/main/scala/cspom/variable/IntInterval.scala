package cspom.variable

import scala.collection.immutable.List
import scala.math.{ max, min }
class IntInterval(val lb: Int, val ub: Int) extends CSPOMDomain[Int] {
  require(ub > lb);

  val getValues = List.range(lb, ub, 1).toSet

  def intersect(domain: CSPOMDomain[_]): CSPOMDomain[_] = domain match {
    case m: IntInterval =>
      new IntInterval(max(lb, m.lb), min(ub, m.ub));
    case _ => domain.intersect(this)
  }

  override def toString = "[" + lb + ".." + ub + "]";
  override val hashCode = 31 * lb + ub;
  override val getSize = 1 + ub - lb

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