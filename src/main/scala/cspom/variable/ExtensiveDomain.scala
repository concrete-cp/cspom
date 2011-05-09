package cspom.variable

class ExtensiveDomain[T](val values: Seq[T]) extends CSPOMDomain[T] {
  override val hashCode = values.hashCode

  override def equals(obj: Any) = obj match {
    case ed: ExtensiveDomain[T] => ed.values == values
    case _ => false
  }

  override def toString =
    if (size > 5)
      values.take(5).mkString("{", ", ", "...}")
    else
      values.mkString("{", ", ", "}")

  def intersect(domain: CSPOMDomain[T]) = new ExtensiveDomain[T](this.values.intersect(domain.values))
}

object ExtensiveDomain {
  def of[T](values: T*) = new ExtensiveDomain(values.toList)
}