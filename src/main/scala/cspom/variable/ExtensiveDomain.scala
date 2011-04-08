package cspom.variable

class ExtensiveDomain[T](val values: List[T]) extends CSPOMDomain[T] {
  override val hashCode = values.hashCode

  override def equals(obj: Any) = obj match {
    case ed: ExtensiveDomain[T] => ed.values == values
    case _ => false
  }

  override def toString =
    if (getSize > 5)
      values.take(5).mkString("(", ", ", "...)")
    else
      values.toString

  def intersect(domain: CSPOMDomain[_]) = domain.intersect(this)
  def getValues = values
}

object ExtensiveDomain {
  def of[T](values: T*) = new ExtensiveDomain(values.toList)
}