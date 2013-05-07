package cspom.variable

class AuxVar(name: String, domain: Option[CSPOMDomain[Any]]) extends CSPOMVariable(name, domain) {
  def this(name: String) = this(name, None)
  def this(domain: CSPOMDomain[Any]) = this(VariableNameGenerator.generate(), None)
  def this() = this(VariableNameGenerator.generate())
  def auxiliary = true
}