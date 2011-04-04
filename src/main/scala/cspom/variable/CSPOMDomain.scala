package cspom.variable

trait CSPOMDomain[T] {
  def getValues: Set[T]
  def getSize: Int = getValues.size
  def intersect(domain: CSPOMDomain[_]): CSPOMDomain[_]
}