package cspom.constraint

import cspom.variable.CSPOMVariable

abstract class CSPOMConstraint(
  val description: String,
  val scope: Seq[CSPOMVariable]) {

  val arity = scope.size
  

  
  val scopeSet = scope.toSet

  //val getScope = JavaConversions.seqAsJavaList(scope)
  //TODO: val positions

  def involves(variable: CSPOMVariable) = scopeSet.contains(variable)

  final def getVariable(position: Int) = scope(position)

//  override def equals(obj: Any): Boolean = obj match {
//    case c: CSPOMConstraint =>
//      scope == c.scope && description == c.description && parameters == c.parameters
//    case _ => false
//  }

  def replacedVar(which: CSPOMVariable, by: CSPOMVariable): CSPOMConstraint;

  def evaluate(t: Seq[Any]): Boolean;
  
  override def toString = description + scope.toString
}
//object CSPOMConstraint {
//  val CONSTRAINT_DESCRIPTION = new com.google.common.base.Function[CSPOMConstraint, String] {
//    override def apply(input: CSPOMConstraint) = input.toString;
//  }
//
//  def matchesDescription(description: String): Predicate[CSPOMConstraint] =
//    Predicates.compose(Predicates.equalTo(description),
//      CONSTRAINT_DESCRIPTION);
//
//}

