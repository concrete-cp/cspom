package cspom.constraint

import javax.script.ScriptException
import cspom.variable.CSPOMVariable
import cspom.Evaluator

class GeneralConstraint(
  description: String,
  parameters: String = null,
  scope: collection.immutable.Seq[CSPOMVariable])
  extends CSPOMConstraint(description, parameters, scope) {

  //  def this(description: String, parameters: String, scope: CSPOMVariable[_]*) =
  //    this(description = description, parameters = parameters,
  //      scope = scope.toList)

  def this(description: String, scope: CSPOMVariable*) =
    this(description = description, scope = scope.toList)

  override def toString = {
    val stb = new StringBuilder
    stb append description

    if (parameters != null) {
      stb append '{' append parameters append '}'
    }

    scope.addString(stb, "(", ", ", ")").toString

  }

  override def evaluate(tuple: Seq[_]): Boolean = {
    val stb = new StringBuilder();
    if (parameters != null) {
      stb append "p_"
    }
    stb append description append '('

    tuple.addString(stb, ", ")

    if (parameters != null) {
      stb append ", " append parameters
    }

    try {
      Evaluator.evaluate((stb append ')').toString);
    } catch {
      case e: ScriptException =>
        throw new IllegalStateException(e);
    }

  }

  override def replacedVar(which: CSPOMVariable, by: CSPOMVariable) = {
    new GeneralConstraint(description, parameters,
      scope map { v => if (v == which) by else v })
  }

}