package cspom.constraint

import cspom.Evaluator
import javax.script.ScriptException
import cspom.variable.CSPOMVariable

class GeneralConstraint(
  name: String = null,
  description: String,
  parameters: String = null,
  val scope: List[CSPOMVariable[_]])
  extends CSPOMConstraint(name, description, parameters) {

  def this(description: String, parameters: String, scope: CSPOMVariable[_]*) =
    this(description = description, parameters = parameters,
      scope = scope.toList)

  def this(description: String, scope: CSPOMVariable[_]*) =
    this(description = description, scope = scope.toList)

  override def toString = {
    val stb = new StringBuilder
    stb append description

    if (parameters != null) {
      stb append '{' append parameters append '}'
    }

    scope.addString(stb, "(", ", ", ")").toString

  }

  override def evaluate(tuple: Any*): Boolean = {
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

  override def replaceVar[T](which: CSPOMVariable[T], by: CSPOMVariable[T]) = {
    new GeneralConstraint(name, description, parameters,
      scope map { v => if (v == which) by else v })
  }

}