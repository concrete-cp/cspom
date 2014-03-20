package cspom

import cspom.variable.IntVariable
import cspom.variable.CSPOMConstant
import cspom.extension.MDD
import cspom.variable.IntDomain
import scala.collection.JavaConversions
import cspom.variable.CSPOMExpression

object JCSPOM {
  def intVarRange(lb: Int, ub: Int) = IntVariable(lb to ub)

  def domainValues(d: IntDomain) = JavaConversions.setAsJavaSet(d)

  def emptyMDD(): MDD[Nothing] = MDD.empty

  def mddAdd[A](mdd: MDD[A], t: Array[A]) = mdd + (t: _*)

  def mdd[A](t: Array[Array[A]]) = MDD[A](t.map(_.toSeq))

  // Required to avoid incompatibility between Int and Integer
  def constant(a: Int) = CSPOMConstant(a)

  def constraint(function: String, arguments: Array[CSPOMExpression[_]], params: ConstraintParameters): CSPOMConstraint[Boolean] =
    CSPOMConstraint(function = Symbol(function), arguments = arguments, params = params)

  def constraint[T](result: CSPOMExpression[T], function: String, arguments: Array[CSPOMExpression[Any]], params: Map[String, Any]): CSPOMConstraint[T] =
    new CSPOMConstraint(result, Symbol(function), arguments.toSeq, params)
}