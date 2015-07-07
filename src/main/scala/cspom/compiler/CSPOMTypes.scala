package cspom.compiler

import cspom.CSPOMConstraint
import cspom.variable.BoolExpression
import cspom.variable.CSPOMSeq

/**
 * @author vion
 */
object CSPOMTypes extends Types {
  def types = {
    case CSPOMConstraint(r, 'clause, Seq(CSPOMSeq(pos), CSPOMSeq(neg)), p) =>
      Map(r -> BoolExpression.coerce(r)) ++
        pos.map(l => l -> BoolExpression.coerce(l)) ++
        neg.map(l => l -> BoolExpression.coerce(l))

  }
}