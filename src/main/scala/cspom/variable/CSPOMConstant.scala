package cspom.variable

object CSPOMTrue extends CSPOMConstant(true) {
  def cspomType = this
  def neg = CSPOMFalse
}

object CSPOMFalse extends CSPOMConstant(true) {
  def cspomType = this
  def neg = CSPOMTrue
}