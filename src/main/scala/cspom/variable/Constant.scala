package cspom.variable

final class Constant[T](val value: T) extends CSPOMDomain[T] {
  override def hashCode = value.hashCode

  override def toString = value.toString

  def values = List(value)

  override val size = 1

  override def equals(that: Any) = that match {
    case other: Constant[T] => other.value == value
    case _ => false
  }

  def intersect[T](domain: CSPOMDomain[T]): CSPOMDomain[T] = {
    domain match {
      case dT: CSPOMDomain[T] =>
        if (dT.values.contains(value)) this;
    }
    throw new IllegalArgumentException("Empty intersection");
  }
}