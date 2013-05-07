package cspom.variable

class ProblemVar(name: String, domain: CSPOMDomain[Any]) extends CSPOMVariable(name, Some(domain)) {
  def auxiliary = false
  //def domain: CSPOMDomain[Any] = super.domain.get
}