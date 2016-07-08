package cspom
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMConstant
import javax.script.ScriptException
import cspom.variable.CSPOMVariable
import scala.collection.JavaConverters._
import scala.collection.mutable.HashMap
import cspom.variable.IntVariable
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMSeq
import cspom.variable.SimpleExpression
import com.typesafe.scalalogging.LazyLogging
import cspom.variable.EmptyVariable
import scala.collection.mutable.WrappedArray

final case class CSPOMConstraint[+T](
    val result: CSPOMExpression[T],
    val function: Symbol,
    val arguments: Seq[CSPOMExpression[Any]],
    val params: Map[String, Any] = Map()) extends Parameterized with LazyLogging {
  
  require(arguments.nonEmpty)

  def withParam(addParams: (String, Any)*) = new CSPOMConstraint(result, function, arguments, params ++ addParams)
  def withParams(addParams: Map[String, Any]) = withParam(addParams.toSeq: _*)

  if (flattenedScope.contains(EmptyVariable)) logger.warn(s"Empty variable in scope of $this")

  def nonReified: Boolean = result.isTrue

  def fullScope: Seq[CSPOMExpression[_]] = result +: arguments

  lazy val flattenedScope: Set[CSPOMExpression[_]] =
    fullScope.iterator.flatMap(_.flatten).toSet

  val id: Int = CSPOMConstraint.id
  CSPOMConstraint.id += 1

  def getArgs: java.util.List[CSPOMExpression[_]] = arguments.asJava

  override final def hashCode: Int = id
  override final def equals(o: Any): Boolean = o match {
    case o: AnyRef => o eq this
    case _ => false
  }

  private def replaceVarShallow[R, S <: R](candidate: CSPOMExpression[R], which: CSPOMExpression[R], by: CSPOMExpression[S]): CSPOMExpression[R] = {
    if (candidate == which) {
      by
    } else {
      candidate
    }
  }

  def replacedVar[R >: T, S >: T <: R](which: CSPOMExpression[R], by: CSPOMExpression[S]): CSPOMConstraint[R] = {
    val newResult = replaceVarShallow(result, which, by)
    val newArgs = arguments.map(replaceVarShallow(_, which, by))

    new CSPOMConstraint(newResult,
      function,
      newArgs,
      params)
  }

  override def toString: String = {
    val args = arguments.map(_.toString)
    if (result.isTrue) {
      toString(None, args)
    } else {
      toString(Some(result.toString), args)
    }
  }

  private def toString(result: Option[String], arguments: Seq[String]): String = {
    val content = s"$id. $function(${arguments.mkString(", ")})$displayParams"
    result match {
      case None => s"constraint $content"
      case Some(r) => s"constraint $r == $content"
    }
  }

  def toString(vn: VariableNames): String = {
    val args = arguments.map(a => vn.names(a))
    if (result.isTrue) {
      toString(None, args)
    } else {
      toString(Some(vn.names(result)), args)
    }

  }

}

object CSPOMConstraint {
  var id = 0

  def param(key: String, v: Any): ConstraintParameters = ConstraintParameters(Map(key -> v))

  //  def apply(function: Symbol, arguments: Seq[CSPOMExpression[_]]): CSPOMConstraint[Boolean] =
  //    CSPOMConstraint(function, arguments, Map[String, Any]())

  //  def apply(function: Symbol, arguments: Seq[CSPOMExpression[_]], params: Map[String, Any]): CSPOMConstraint[Boolean] =
  //    new CSPOMConstraint(CSPOMConstant(true), function, arguments, params)
  def apply(function: Symbol)(arguments: CSPOMExpression[_]*): CSPOMConstraint[Boolean] =
    apply(CSPOMConstant(true))(function)(arguments: _*)

  def apply[R](result: CSPOMExpression[R])(function: Symbol)(arguments: CSPOMExpression[_]*): CSPOMConstraint[R] =
    new CSPOMConstraint(result, function, arguments)

}

case class ConstraintParameters(m: Map[String, Any]) extends Map[String, Any] {
  def param(key: String, v: Any): ConstraintParameters = ConstraintParameters(m + (key -> v))
  def +[B1 >: Any](kv: (String, B1)): Map[String, B1] = ConstraintParameters(m + kv)
  def -(key: String): scala.collection.immutable.Map[String, Any] = ConstraintParameters(m - key)
  def get(key: String): Option[Any] = m.get(key)
  def iterator: Iterator[(String, Any)] = m.iterator
}

