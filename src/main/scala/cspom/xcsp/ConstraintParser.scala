package cspom.xcsp

import scala.annotation.elidable.ASSERTION
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharSequenceReader
import cspom.CSPOM
import cspom.variable.CSPOMVariable
import cspom.variable.CSPOMExpression
import cspom.CSPOMConstraint
import cspom.variable.IntVariable
import cspom.variable.CSPOMConstant
import cspom.variable.FreeVariable
import java.util.StringTokenizer
import scala.collection.mutable.ArrayBuffer
import cspom.extension.Table
import scala.util.parsing.input.Reader
import cspom.extension.MDD
import cspom.extension.Relation
import cspom.variable.SimpleExpression

sealed trait PredicateNode

final case class PredicateConstraint(val operator: String, val arguments: Seq[PredicateNode]) extends PredicateNode {
  require(arguments.nonEmpty)
}
final case class PredicateConstant(val constant: Int) extends PredicateNode

final case class PredicateVariable(val variableId: String) extends PredicateNode

final object ConstraintParser extends JavaTokenParsers {

  private def integer = wholeNumber ^^ (_.toInt)

  def func: Parser[PredicateNode] =
    ident ~ ("(" ~> repsep(func, ",") <~ ")") ^^ {
      case ident ~ children => PredicateConstraint(ident, children)
    } |
      ident ^^ (PredicateVariable(_)) |
      integer ^^ (PredicateConstant(_))

  def mapFunc(map: Map[String, String]): Parser[String] =
    ident ~ ("(" ~> repsep(mapFunc(map), ",") <~ ")") ^^ {
      case ident ~ children => ident + children.mkString("(", ", ", ")")
    } |
      ident ^^ (map(_)) |
      integer ^^ (_.toString)

  def split(expression: String, declaredVariables: Map[String, SimpleExpression[Int]], cspom: CSPOM): Unit = {
    func(new CharSequenceReader(expression)).get match {
      case PredicateConstraint(operator, arguments) =>
        cspom.ctr(CSPOMConstraint(Symbol(operator))(arguments.map(toVariable(_, declaredVariables, cspom)): _*))

      case _ => throw new IllegalArgumentException("Constraint expected, was " + expression)
    }

  }

  private def toVariable(node: PredicateNode, declaredVariables: Map[String, SimpleExpression[Int]], cspom: CSPOM): SimpleExpression[_] = {
    node match {
      case PredicateConstant(value)      => CSPOMConstant(value)
      case PredicateVariable(variableId) => declaredVariables(variableId)
      case PredicateConstraint(operator, arguments) =>
        cspom.is(Symbol(operator), arguments.map(toVariable(_, declaredVariables, cspom)))
    }
  }

  @annotation.tailrec
  private def readerToString(r: Reader[Char], stb: StringBuilder = new StringBuilder): String =
    if (r.atEnd) {
      stb.toString
    } else {
      readerToString(r.rest, stb.append(r.first))
    }

  def parseTable(reader: Reader[Char], arity: Int, size: Int): Relation[Int] = {

    val text = readerToString(reader)

    val st = new StringTokenizer(text, "|")

    var table = MDD.empty[Int]

    while (st.hasMoreTokens) {
      val t = st.nextToken().trim.split(" +")
      require(t.length == arity, t.toSeq.toString)
      table += t.map(_.toInt)
    }

    table.reduce
  }

}
