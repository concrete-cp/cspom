package cspom.variable

final class Constant[T](val value: T) extends CSPOMDomain[T] {
  override def hashCode = value.hashCode

  override def toString = value.toString

  def values = List(value)

  override val size = 1

  def contains(v: Any) = v == value

  override def equals(that: Any) = that match {
    case other: Constant[T] => other.value == value
    case _ => false
  }

  def toXCSP = value.toString

  def intersect[B](domain: CSPOMDomain[B]): CSPOMDomain[B] = {
    require(domain.isInstanceOf[CSPOMDomain[T]] && domain.values.contains(value))
    this.asInstanceOf[CSPOMDomain[B]]
  }
}