package cspom.constraint

import javax.script.ScriptException
import cspom.variable.CSPOMVariable
import cspom.Evaluator

class GeneralConstraint(
  val predicate: Predicate,
  scope: Seq[CSPOMVariable])
  extends CSPOMConstraint(predicate.function, scope) {

  require(scope.nonEmpty, "The constraint must involve at least one variable")
  //  def this(description: String, parameters: String, scope: CSPOMVariable[_]*) =
  //    this(description = description, parameters = parameters,
  //      scope = scope.toList)

  def this(func: String, params: Any, scope: CSPOMVariable*) =
    this(Predicate(func, Some(params)), scope)

  def this(func: String, scope: CSPOMVariable*) =
    this(Predicate(func, None), scope)

  override def toString = {
    val stb = new StringBuilder
    stb append description
    stb.append(predicate.optParameters)
    scope.addString(stb, "(", ", ", ")").toString
  }

  override def evaluate(tuple: Seq[_]): Boolean = {
    val stb = new StringBuilder();
    if (predicate.parameters.isDefined) {
      stb append "p_"
    }
    stb append description append '('

    tuple.addString(stb, ", ")

    predicate.parameters foreach {
      case s: Seq[Any] =>
        stb.append(", ")
        s.addString(stb, ", ")
      case p: Any =>
        stb.append(", ").append(p)
    }

    Evaluator.evaluate((stb append ')').toString);
  }

  override def replacedVar(which: CSPOMVariable, by: CSPOMVariable) = {
    new GeneralConstraint(predicate,
      scope map { v => if (v == which) by else v })
  }

}