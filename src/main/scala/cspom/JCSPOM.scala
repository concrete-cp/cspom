package cspom

import cspom.variable.IntVariable
import cspom.variable.CSPOMConstant
import cspom.extension.MDD
import cspom.variable.IntDomain
import scala.collection.JavaConversions

object JCSPOM {
  def intVarRange(lb: Int, ub: Int) = IntVariable(lb to ub)

  def domainValues(d: IntDomain) = JavaConversions.seqAsJavaList(d)

  def emptyMDD(): MDD = MDD.empty

  def mddAdd(mdd: MDD, t: Array[Int]) = mdd + (t: _*)

  // Required to avoid incompatibility between Int and Integer
  def constant(a: Int) = CSPOMConstant(a)
}