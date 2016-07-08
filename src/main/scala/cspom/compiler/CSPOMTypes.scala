package cspom.compiler

import cspom.CSPOMConstraint
import cspom.variable.BoolExpression
import cspom.variable.CSPOMSeq
import cspom.variable.FreeVariable
import cspom.variable.CSPOMExpression
import cspom.variable.IntExpression
import cspom.variable.IntVariable
import cspom.util.IntInterval

/**
 * @author vion
 */
object CSPOMTypes extends Types {
  def types = {
    case CSPOMConstraint(r, 'clause, Seq(CSPOMSeq(pos), CSPOMSeq(neg)), p) =>
      Map(r -> coerce(r)) ++
        pos.map(l => l -> coerce(l)) ++
        neg.map(l => l -> coerce(l))

  }

  private def coerce(e: CSPOMExpression[_]) = e match {
    case e: FreeVariable => BoolExpression.coerce(e)
    case IntExpression(e) => reduceDomain(e, IntInterval(0, 1))
    case e => e
  }
}