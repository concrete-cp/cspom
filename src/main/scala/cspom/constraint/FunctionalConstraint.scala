package cspom.constraint

import cspom.{ Evaluator, Loggable }
import cspom.variable.CSPOMVariable
import javax.script.ScriptException

class FunctionalConstraint(
  val result: CSPOMVariable[_],
  val function: String,
  parameters: String = null,
  val arguments: Seq[CSPOMVariable[_]])
  extends CSPOMConstraint(function, parameters, result +: arguments)
  with Loggable {
  require(result != null)
  require(arguments != null)
  require(!arguments.isEmpty, "Must have at least one argument")

  def this(result: CSPOMVariable[_], function: String, arguments: CSPOMVariable[_]*) =
    this(result = result, function = function, arguments = arguments.toList)

  override def toString = {
    val stb = new StringBuilder
    stb append result append " = " append description
    if (parameters != null) {
      stb append '{' append parameters append '}'
    }
    arguments.addString(stb, "(", ", ", ")").toString
  }

  override def replacedVar[T](which: CSPOMVariable[T], by: CSPOMVariable[T]) = {

    new FunctionalConstraint({ if (which == result) by else result },
      function, parameters,
      arguments map { v => if (v == which) by else v })

  }

  override def evaluate(tuple: Seq[_]): Boolean = {
    val stb = new StringBuilder
    stb append tuple(0) append " == " append description append '('

    tuple.tail.addString(stb, ", ");

    if (parameters != null) {
      stb append ", " append parameters;
    }

    try {
      Evaluator.evaluate((stb append ")").toString());
    } catch {
      case e: ScriptException =>
        throwing(Evaluator.getClass.getName, "evaluate", e);
        throw new IllegalStateException(e);
    }

  }

}