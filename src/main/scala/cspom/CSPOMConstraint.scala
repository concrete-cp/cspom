package cspom
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMConstant
import javax.script.ScriptException
import cspom.variable.CSPOMVariable
import scala.collection.JavaConversions
import scala.collection.mutable.HashMap
import cspom.variable.IntVariable
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMSeq
import cspom.variable.SimpleExpression

final case class CSPOMConstraint[+T](
  val result: CSPOMExpression[T],
  val function: Symbol,
  val arguments: Seq[CSPOMExpression[Any]],
  val params: Map[String, Any] = Map()) extends Parameterized with Loggable {

  require(result != null)
  require(arguments != null)
  require(arguments.nonEmpty, "Must have at least one argument")

  require(function != 'or || !arguments.forall {
    case CSPOMConstant(c: Boolean) => !c
    case _ => false
  })

  def fullScope = result +: arguments

  val id = CSPOMConstraint.id
  CSPOMConstraint.id += 1

  def getArgs = JavaConversions.seqAsJavaList(arguments)

  override final def hashCode = id
  override final def equals(o: Any) = o match {
    case o: AnyRef => o eq this
    case _ => false
  }

  def replacedVar[R, S <: R](which: CSPOMExpression[R], by: CSPOMExpression[S]) = {
    val newResult = result.replaceVar(which, by)
    val newArgs: Seq[CSPOMExpression[Any]] = arguments.map(_.replaceVar(which, by))

    new CSPOMConstraint(newResult,
      function,
      newArgs,
      params)
  }

  override def toString = {
    val args = arguments.map(_.toString)
    if (result == CSPOMConstant(true)) {
      toString(None, args)
    } else {
      toString(Some(result.toString), args)
    }
  }

  private def toString(result: Option[String], arguments: Seq[String]): String = {
    val content = s"$function(${arguments.mkString(", ")})$displayParams"
    result match {
      case None => s"constraint $content"
      case Some(r) => s"constraint $r == $content"
    }
  }

  def toString(vn: VariableNames): String = {
    val args = arguments.map(vn.names(_))
    if (result == CSPOMConstant(true)) {
      toString(None, args)
    } else {
      toString(Some(vn.names(result)), args)
    }

  }

}

object CSPOMConstraint {
  var id = 0

  def param(key: String, v: Any) = ConstraintParameters(Map(key -> v))

  def apply[T](result: CSPOMExpression[T], function: Symbol, arguments: CSPOMExpression[Any]*): CSPOMConstraint[T] =
    new CSPOMConstraint(result, function, arguments)

  def apply(function: Symbol, arguments: Seq[CSPOMExpression[_]], params: Map[String, Any] = Map()): CSPOMConstraint[Boolean] =
    new CSPOMConstraint(CSPOMConstant(true), function, arguments, params)

  def apply(function: Symbol, arguments: CSPOMExpression[_]*): CSPOMConstraint[Boolean] =
    apply(function, arguments)

  /**
   *  For Java interop
   */
  def apply(function: String, arguments: Array[CSPOMExpression[_]], params: ConstraintParameters): CSPOMConstraint[Boolean] =
    apply(Symbol(function), arguments, params)

  def apply[T](result: CSPOMExpression[T], function: String, arguments: Array[CSPOMExpression[Any]], params: Map[String, Any]): CSPOMConstraint[T] =
    new CSPOMConstraint(result, Symbol(function), arguments.toSeq, params)
}

final class VariableNames(cspom: CSPOM) {

  val generatedNames = new HashMap[CSPOMExpression[_], String]

  for ((n, v) <- cspom.namedExpressions) {
    name(v, n)
  }

  private def add(e: CSPOMExpression[_], n: String) {
    generatedNames(e) = generatedNames.get(e) match {
      case Some(ns) => s"$ns||$n"
      case None => n
    }
  }

  private def name(e: CSPOMExpression[_], root: String): Unit = {
    add(e, root)

    e match {
      case CSPOMSeq(v, i, _) => for ((v, i) <- (v zip i)) {
        name(v, s"$root[$i]")
      }
      case _ =>
    }
  }

  var id = 0

  private def nextName(e: CSPOMExpression[_]) = e match {
    case CSPOMConstant(v) => v.toString
    case CSPOMSeq(v, _, _) => v.map(names).mkString("CSPOMSeq(", ", ", ")")
    case _ =>
      id += 1
      "_" + id
  }

  def names(expression: CSPOMExpression[_]): String =
    generatedNames.getOrElseUpdate(expression, nextName(expression))
}

case class ConstraintParameters(m: Map[String, Any]) extends Map[String, Any] {
  def param(key: String, v: Any) = ConstraintParameters(m + (key -> v))
  def +[B1 >: Any](kv: (String, B1)): Map[String, B1] = ConstraintParameters(m + kv)
  def -(key: String): scala.collection.immutable.Map[String, Any] = ConstraintParameters(m - key)
  def get(key: String): Option[Any] = m.get(key)
  def iterator: Iterator[(String, Any)] = m.iterator
}

