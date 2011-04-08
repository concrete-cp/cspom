package cspom.variable

final class Constant[T](val value: T) extends CSPOMDomain[T] {
  override def hashCode = value.hashCode

  override def toString = value.toString

  def getValues = List(value)

  override def getSize = 1

  override def equals(that: Any) = that match {
    case other: Constant[T] => other.value == value
    case _ => false
  }

  def intersect(domain: CSPOMDomain[_]): CSPOMDomain[_] = {
    domain match {
      case dT: CSPOMDomain[T] =>
        if (dT.getValues.contains(value)) this;
    }
    throw new IllegalArgumentException("Empty intersection");
  }
}