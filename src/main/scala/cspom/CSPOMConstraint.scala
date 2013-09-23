package cspom
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMTrue
import javax.script.ScriptException
import cspom.variable.CSPOMVariable

final class CSPOMConstraint(
  val result: CSPOMExpression,
  val function: String,
  val arguments: Seq[CSPOMExpression],
  val params: Map[String, Any] = Map()) extends Loggable {

  require(result != null)
  require(arguments != null)
  require(arguments.nonEmpty, "Must have at least one argument")

  /**
   *  Warning: scope is not ordered! Use fullScope to get ordered
   *  information.
   */
  lazy val scope = fullScope.flatMap(_.flattenVariables).toSet

  def fullScope = result +: arguments

  def arity = scope.size

  val id = CSPOMConstraint.id
  CSPOMConstraint.id += 1

  def this(result: CSPOMExpression, function: String, arguments: CSPOMExpression*) =
    this(result, function, arguments)

  def this(function: String, arguments: CSPOMExpression*) =
    this(CSPOMTrue, function, arguments)

  def this(function: String, arguments: Seq[CSPOMExpression], params: Map[String, Any]) =
    this(CSPOMTrue, function, arguments, params)

  //val scopeSet = scope.toSet

  //val getScope = JavaConversions.seqAsJavaList(scope)
  //TODO: val positions

  //  def involves(variable: CSPOMVariable) = scopeSet.contains(variable)
  //
  //  final def getVariable(position: Int) = scope(position)

  override final def hashCode = id
  override final def equals(o: Any) = o match {
    case o: AnyRef => o eq this
    case _ => false
  }

  def replacedVar(which: CSPOMVariable, by: CSPOMExpression) =
    new CSPOMConstraint(result.replaceVar(which, by),
      function,
      arguments map { v => v.replaceVar(which, by) },
      params)

  def evaluate(tuple: Seq[Any]): Boolean = {
    val stb = new StringBuilder
    stb.append(tuple.head).append(" == ").append(function)

    tuple.tail.addString(stb, "(", ", ", ")");

    //    if (predicate.parameters.isDefined) {
    //      stb append ", " append predicate.parameters.get;
    //    }

    try {
      Evaluator.evaluate(stb.toString);
    } catch {
      case e: ScriptException =>
        throwing(Evaluator.getClass.getName, "evaluate", e);
        sys.error(stb.toString);
    }

  }

  override def toString = {
    val content = s"$function(${arguments.mkString(", ")})${if (params.isEmpty) "" else params.mkString(" :: ", " :: ", "")}"
    result match {
      case CSPOMTrue => s"constraint $content"
      case _ => s"constraint $result == $content"
    }
  }
}

object CSPOMConstraint {
  var id = 0
}

