package cspom.flatzinc

import cspom.variable.CSPOMVariable
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMFalse
import cspom.variable.CSPOMTrue

sealed trait FZExpr {
  type VarMap = Map[String, CSPOMVariable]
  type SeqMap = Map[String, CSPOMSeq[CSPOMExpression]]

  def toCSPOM(vars: VarMap, seqs: SeqMap): CSPOMExpression
}

case class FZBoolConst(value: Boolean) extends FZExpr {
  def toCSPOM(vars: VarMap, seqs: SeqMap) = {
    if (value) CSPOMTrue else CSPOMFalse
  }
}

