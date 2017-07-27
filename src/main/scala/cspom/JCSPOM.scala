package cspom

import cspom.extension.MDDRelation
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMExpression
import cspom.variable.IntVariable

object JCSPOM {
  def intVarRange(lb: Int, ub: Int) = IntVariable(lb to ub)

  //  def domainValues(d: IntDomain) = d match {
  //    case i: IntIntervals => JavaConversions.setAsJavaSet(i.intervals)
  //    case FreeInt => throw new UnsupportedOperationException
  //  }

  def emptyMDD(): MDDRelation = MDDRelation.empty

  // def mddAdd(mdd: MDDRelation, t: Array[Int]) = mdd + t

  def mdd[A](t: Array[Array[Int]]) = MDDRelation(t)

  // Required to avoid incompatibility between Int and Integer
  def constant(a: Int) = CSPOMConstant(a)

  def constraint(function: String, arguments: Array[CSPOMExpression[_]], params: ConstraintParameters): CSPOMConstraint[Boolean] =
    CSPOMConstraint(Symbol(function))(arguments: _*) withParam (params.toSeq: _*)

  def constraint[T](result: CSPOMExpression[T], function: String, arguments: Array[CSPOMExpression[Any]], params: Map[String, Any]): CSPOMConstraint[T] =
    new CSPOMConstraint(result, Symbol(function), arguments.toSeq, params)
}