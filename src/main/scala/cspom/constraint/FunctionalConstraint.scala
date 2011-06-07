package cspom.constraint

import cspom.variable.CSPOMVariable
import cspom.{ Evaluator, Loggable }
import javax.script.ScriptException
import scala.collection.JavaConversions

class FunctionalConstraint(
  val result: CSPOMVariable,
  val function: String,
  parameters: String = null,
  val arguments: collection.immutable.Seq[CSPOMVariable])
  extends CSPOMConstraint(function, parameters, result +: arguments)
  with Loggable {
  require(result != null)
  require(arguments != null)
  require(!arguments.isEmpty, "Must have at least one argument")

  def this(result: CSPOMVariable, function: String, arguments: CSPOMVariable*) =
    this(result = result, function = function, arguments = arguments.toList)

  val getArguments = JavaConversions.seqAsJavaList(arguments)

  override def toString = {
    val stb = new StringBuilder
    stb append result append " = " append description
    if (parameters != null) {
      stb append '{' append parameters append '}'
    }
    arguments.addString(stb, "(", ", ", ")").toString
  }

  override def replacedVar(which: CSPOMVariable, by: CSPOMVariable) = 
    new FunctionalConstraint({ if (which == result) by else result },
      function, parameters,
      arguments map { v => if (v == which) by else v })
  

  override def evaluate(tuple: Seq[Any]): Boolean = {
    val stb = new StringBuilder
    stb append tuple(0) append " == " append description append '('

    tuple.tail.addString(stb, ", ");

    if (parameters != null) {
      stb append ", " append parameters;
    }

    try {
      Evaluator.evaluate((stb append ")").toString);
    } catch {
      case e: ScriptException =>
        throwing(Evaluator.getClass.getName, "evaluate", e);
        sys.error(stb.toString);
    }

  }

}