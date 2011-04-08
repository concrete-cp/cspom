package cspom.constraint

import com.google.common.base.Predicates
import cspom.variable.CSPOMVariable
import cspom.{ Evaluator, Loggable }
import javax.script.ScriptException

class FunctionalConstraint(val result: CSPOMVariable[_],
  function: String, parameters: String, val arguments: List[CSPOMVariable[_]])
  extends CSPOMConstraint(description = function, parameters = parameters, scope = result :: arguments)
  with Loggable {
  require(!arguments.isEmpty, "Must have at least one argument")

  def this(result: CSPOMVariable[_], function: String, parameters: String, arguments: Array[CSPOMVariable[_]]) =
    this(result, function, parameters, arguments.toList)

  def this(result: CSPOMVariable[_], function: String, parameters: String,
    arguments: CSPOMVariable[_]*) = this(result, function, parameters,
    arguments.toList)

  override def toString = {
    val stb = new StringBuilder
    stb.append(result).append(" = ").append(description);
    if (parameters != null) {
      stb.append('{').append(parameters).append('}');
    }
    arguments.addString(stb, "(", ", ", ")").toString
  }

  override def replaceVar[T](which: CSPOMVariable[T], by: CSPOMVariable[T]) = {
    if (which == result) {
      new FunctionalConstraint(by, function, parameters, arguments)
    } else {
      new FunctionalConstraint(result, function, parameters,
        arguments.map((v: CSPOMVariable[_]) => if (v == which) by else v))
    }
  }

  override def evaluate(tuple: Any*): Boolean = {
    val stb = new StringBuilder();
    stb.append(tuple(0)).append(" == ").append(description)
      .append('(');
    tuple.tail.addString(stb, ", ");

    if (parameters != null) {
      stb.append(", ").append(parameters);
    }

    try {
      Evaluator.evaluate(stb.append(")").toString());
    } catch {
      case e: ScriptException => 
        throwing(classOf[Evaluator].getName, "evaluate", e);
	throw new IllegalStateException(e);
    }

  }

}

// object FunctionalConstraint {
//   def matchesDescription(description: String) = Predicates.and(
//     CSPOMConstraint.matchesDescription(description),
//     Predicates.instanceOf(classOf[FunctionalConstraint]));
// }
