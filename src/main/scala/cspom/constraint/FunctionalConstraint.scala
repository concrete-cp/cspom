package cspom.constraint

import com.google.common.base.{ Predicate, Predicates }
import cspom.variable.CSPOMVariable
import cspom.{ Evaluator, Loggable }
import javax.script.ScriptException

class FunctionalConstraint(
  name: String = null,
  val result: CSPOMVariable[_],
  val function: String,
  parameters: String,
  val arguments: List[CSPOMVariable[_]])
  extends CSPOMConstraint(
    name = name,
    description = function,
    parameters = parameters)
  with Loggable {
  require(!arguments.isEmpty, "Must have at least one argument")

  def this(result: CSPOMVariable[_], function: String, parameters: String,
    arguments: CSPOMVariable[_]*) =
    this(result = result, function = function, parameters = parameters,
      arguments = arguments.toList)

  override val scope = result :: arguments

  override def toString = {
    val stb = new StringBuilder
    stb append result append " = " append description
    if (parameters != null) {
      stb append '{' append parameters append '}'
    }
    arguments.addString(stb, "(", ", ", ")").toString
  }

  override def replaceVar[T](which: CSPOMVariable[T], by: CSPOMVariable[T]) = {

    new FunctionalConstraint(name, { if (which == result) by else result },
      function, parameters,
      arguments map { v => if (v == which) by else v })

  }

  override def evaluate(tuple: Any*): Boolean = {
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
        throwing(classOf[Evaluator].getName, "evaluate", e);
        throw new IllegalStateException(e);
    }

  }

}
//
//object FunctionalConstraint {
//  def matchesDescription(description: String): Predicate[_ >: CSPOMConstraint] =
//    Predicates.and(Predicates.instanceOf(classOf[FunctionalConstraint]),
//      CSPOMConstraint.matchesDescription(description))
//}
