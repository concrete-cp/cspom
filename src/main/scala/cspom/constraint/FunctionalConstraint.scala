package cspom.constraint

import cspom.variable.CSPOMVariable
import cspom.{ Evaluator, Loggable }
import javax.script.ScriptException
import scala.collection.JavaConversions

class FunctionalConstraint(
  val result: CSPOMVariable,
  val predicate: Predicate,
  val arguments: Seq[CSPOMVariable])
  extends CSPOMConstraint(predicate.function, result +: arguments)
  with Loggable {
  require(result != null)
  require(arguments != null)
  require(arguments.nonEmpty, "Must have at least one argument")

  def this(result: CSPOMVariable, function: String, arguments: CSPOMVariable*) =
    this(result, Predicate(function, None), arguments)

  def this(result: CSPOMVariable, func: String, params: Any, args: CSPOMVariable*) =
    this(result, Predicate(func, Some(params)), args)

  override def toString = {
    val stb = new StringBuilder
    stb.append(result).append(" = ").append(predicate.function)
    stb.append(predicate.optParameters)
    arguments.addString(stb, "(", ", ", ")").toString
  }

  override def replacedVar(which: CSPOMVariable, by: CSPOMVariable) =
    new FunctionalConstraint({ if (which == result) by else result },
      Predicate(predicate.function, predicate.parameters),
      arguments map { v => if (v == which) by else v })

  override def evaluate(tuple: Seq[Any]): Boolean = {
    val stb = new StringBuilder
    stb append tuple(0) append " == " append description append '('

    tuple.tail.addString(stb, ", ");

    if (predicate.parameters.isDefined) {
      stb append ", " append predicate.parameters.get;
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