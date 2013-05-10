package cspom.constraint

import cspom.variable.CSPOMVariable

case class Predicate(
  val function: String,
  val parameters: Option[Any]) {
  require(parameters.isEmpty || parameters.get != null)

  def optParameters = parameters map { p => s"{$p}" } getOrElse { "" }

}
