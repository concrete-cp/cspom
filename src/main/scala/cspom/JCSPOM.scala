package cspom

import cspom.variable.IntVariable
import cspom.variable.CSPOMConstant
import cspom.extension.MDD

object JCSPOM {
  def intVarRange(lb: Int, ub: Int) = IntVariable(lb to ub)

  def emptyMDD() = MDD.empty
}