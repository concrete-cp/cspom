package cspom.compiler

import cspom.CSPOMConstraint
import cspom.variable.BoolExpression
import cspom.variable.CSPOMSeq
import cspom.variable.FreeVariable
import cspom.variable.CSPOMExpression
import cspom.variable.IntExpression
import cspom.util.IntInterval

/**
 * @author vion
 */
object CSPOMTypes extends Types {

  def functions = Functions('clause)

  def types: PartialFunction[CSPOMConstraint[_], A] = {
    case CSPOMConstraint(r, 'clause, Seq(CSPOMSeq(pos), CSPOMSeq(neg)), p) =>
      Map(r -> coerce(r)) ++
        pos.map(l => l -> coerce(l)) ++
        neg.map(l => l -> coerce(l))

  }

  private def coerce(e: CSPOMExpression[_]) = e match {
    case e: FreeVariable => BoolExpression.coerce(e)
    case IntExpression(e) => ConstraintCompiler.reduceDomain(e, IntInterval(0, 1))
    case e => e
  }
}