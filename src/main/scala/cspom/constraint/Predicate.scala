package cspom.constraint

import cspom.variable.CSPOMVariable

case class Predicate(
  val function: String,
  val parameters: Option[String]) {
  require(parameters.isEmpty || parameters.get != null)
  def optParameters = parameters match {
    case Some(p) => "{" + p + "}"
    case None => ""
  }
}
