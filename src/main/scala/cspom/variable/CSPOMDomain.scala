package cspom.variable

trait CSPOMDomain[T] {
  def getValues: List[T]
  def getSize: Int = getValues.size
  def intersect(domain: CSPOMDomain[T]): CSPOMDomain[T]
}