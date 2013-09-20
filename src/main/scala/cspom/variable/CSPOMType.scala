package cspom.variable

trait CSPOMType {
  def generalizes(other: CSPOMType): Boolean
}

object CSPOMFree extends CSPOMType {
  def generalizes(other: CSPOMType) = true
}
object CSPOMInt extends CSPOMType {
  def generalizes(other: CSPOMType) = other == CSPOMInt
}
object CSPOMDouble extends CSPOMType {
  def generalizes(other: CSPOMType) = other == CSPOMDouble
}
object CSPOMBool extends CSPOMType {
  private lazy val spec = Set[CSPOMType](CSPOMBool, CSPOMTrue, CSPOMFalse)
  def generalizes(other: CSPOMType) = spec.contains(other)
}

case class CSPOMSeqType(content: CSPOMType) extends CSPOMType {
  def generalizes(other: CSPOMType) = other match {
    case CSPOMSeqType(c) => c.generalizes(content)
    case _ => false
  }
}
