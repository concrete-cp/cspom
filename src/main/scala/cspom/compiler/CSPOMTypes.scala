package cspom.compiler

import cspom.CSPOMConstraint
import cspom.util.IntInterval
import cspom.variable._

import scala.collection.immutable

/**
  * @author vion
  */
object CSPOMTypes extends Types {

  def functions = Functions("clause")

  def types: PartialFunction[CSPOMConstraint[_], A] = {
    case CSPOMConstraint(r, "clause", Seq(CSPOMSeq(pos), CSPOMSeq(neg)), p) =>
      ((r -> coerce(r)) +: pos.map(l => l -> coerce(l)) ++: neg.map(l => l -> coerce(l))).toMap
  }

  private def coerce(e: CSPOMExpression[_]) = e match {
    case e: FreeVariable => BoolExpression.coerce(e)
    case IntExpression(e) => ConstraintCompiler.reduceDomain(e, IntInterval(0, 1))
    case e => e
  }
}