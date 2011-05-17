package cspom.constraint

import javax.script.ScriptException
import cspom.variable.CSPOMVariable
import cspom.Evaluator

class GeneralConstraint(
  description: String,
  parameters: String = null,
  scope: Seq[CSPOMVariable[Any]])
  extends CSPOMConstraint(description, parameters, scope) {

  //  def this(description: String, parameters: String, scope: CSPOMVariable[_]*) =
  //    this(description = description, parameters = parameters,
  //      scope = scope.toList)

  def this(description: String, scope: CSPOMVariable[Any]*) =
    this(description = description, scope = scope)

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

  override def replacedVar[T >: Any](which: CSPOMVariable[T], by: CSPOMVariable[T]) = {
    new GeneralConstraint(description, parameters,
      scope map { v => if (v == which) by else v })
  }

}