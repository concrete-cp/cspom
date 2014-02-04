package cspom
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMTrue
import javax.script.ScriptException
import cspom.variable.CSPOMVariable
import scala.collection.JavaConversions

final case class CSPOMConstraint(
  val result: CSPOMExpression,
  val function: Symbol,
  val arguments: Seq[CSPOMExpression],
  val params: Map[String, Any] = Map()) extends Loggable {

  require(result != null)
  require(arguments != null)
  require(arguments.nonEmpty, "Must have at least one argument")

  def this(result: CSPOMExpression, function: String, arguments: Array[CSPOMExpression], params: Map[String, Any]) =
    this(result, Symbol(function), arguments.toSeq, params)

  def this(result: CSPOMExpression, function: Symbol, arguments: CSPOMExpression*) =
    this(result, function, arguments)

  def this(function: Symbol, arguments: Seq[CSPOMExpression], params: Map[String, Any] = Map()) =
    this(CSPOMTrue, function, arguments, params)

  def this(function: String, arguments: Array[CSPOMExpression], params: Map[String, Any]) =
    this(Symbol(function), arguments, params)

  def this(function: Symbol, arguments: CSPOMExpression*) =
    this(function, arguments)

  def fullScope = result +: arguments

  val id = CSPOMConstraint.id
  CSPOMConstraint.id += 1

  def getParam[A](name: String, typ: Class[A]): Option[A] =
    try {
      params.get(name).map(typ.cast)
    } catch {
      case e: ClassCastException =>
        throw new IllegalArgumentException("Could not cast " + params(name) + ": " + params(name).getClass + " to " + typ)
    }

  def getArgs = JavaConversions.seqAsJavaList(arguments)

  override final def hashCode = id
  override final def equals(o: Any) = o match {
    case o: AnyRef => o eq this
    case _ => false
  }

  def replacedVar(which: CSPOMExpression, by: CSPOMExpression) =
    new CSPOMConstraint(result.replaceVar(which, by),
      function,
      arguments map { v => v.replaceVar(which, by) },
      params)

  override def toString = {
    val args = arguments.map(_.toString)
    result match {
      case CSPOMTrue => toString(None, args)
      case r: CSPOMExpression => toString(Some(r.toString), args)
    }
  }

  private def name(e: CSPOMExpression, cspom: CSPOM): String = {
    cspom.expressionNames.getOrElse(e, e.toString)
  }

  private def toString(result: Option[String], arguments: Seq[String]): String = {
    val content = s"$function(${arguments.mkString(", ")})${if (params.isEmpty) "" else params.mkString(" :: ", " :: ", "")}"
    result match {
      case None => s"constraint $content"
      case Some(r) => s"constraint $r == $content"
    }
  }

  def toString(cspom: CSPOM): String = {
    val args = arguments.map(name(_, cspom))
    result match {
      case CSPOMTrue => toString(None, args)
      case r: CSPOMExpression => toString(Some(name(r, cspom)), args)
    }

  }

}

object CSPOMConstraint {
  var id = 0

  def param(key: String, v: Any) = ConstraintParameters(Map(key -> v))
}

case class ConstraintParameters(m: Map[String, Any]) extends Map[String, Any] {
  def param(key: String, v: Any) = ConstraintParameters(m + (key -> v))
  def +[B1 >: Any](kv: (String, B1)): Map[String, B1] = ConstraintParameters(m + kv)
  def -(key: String): scala.collection.immutable.Map[String, Any] = ConstraintParameters(m - key)
  def get(key: String): Option[Any] = m.get(key)
  def iterator: Iterator[(String, Any)] = m.iterator
}

