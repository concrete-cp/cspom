package cspom

import cspom.variable.IntVariable
import cspom.variable.CSPOMConstant
import cspom.extension.MDD
import cspom.variable.IntDomain
import scala.collection.JavaConversions

object JCSPOM {
  def intVarRange(lb: Int, ub: Int) = IntVariable(lb to ub)

  def domainValues(d: IntDomain) = JavaConversions.seqAsJavaList(d)

  def emptyMDD(): MDD[Nothing] = MDD.empty

  def mddAdd[A](mdd: MDD[A], t: Array[A]) = mdd + (t: _*)

  def mdd[A](t: Array[Array[A]]) = MDD[A](t.map(_.toSeq): _*)

  // Required to avoid incompatibility between Int and Integer
  def constant(a: Int) = CSPOMConstant(a)
}