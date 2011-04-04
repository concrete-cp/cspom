package cspom.variable

final class Constant[T](val value: T) extends CSPOMDomain[T] {
  override def hashCode = value.hashCode

  override def toString = value.toString

  def getValues = Set(value)

  override def getSize = 1

  override def equals(that: Any) = that match {
    case other: Constant[T] => other.value == value
    case _ => false
  }

  def intersect[E >: T](domain: CSPOMDomain[E]): CSPOMDomain[_] = {
    if (domain.getValues.contains(value)) {
      return this;
    }
    throw new IllegalArgumentException("Empty intersection");
  }
}