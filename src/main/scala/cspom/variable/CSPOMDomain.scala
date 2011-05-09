package cspom.variable

trait CSPOMDomain[T] {
  def values: Seq[T]
  def size: Int = values.size
  def intersect(domain: CSPOMDomain[T]): CSPOMDomain[T]
}