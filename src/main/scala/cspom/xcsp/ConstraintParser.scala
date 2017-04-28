package cspom.xcsp

import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.input.CharSequenceReader
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.variable.CSPOMConstant
import java.util.StringTokenizer
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
      case PredicateConstraint(operator, arguments) => cspom.defineFree(result =>
        CSPOMConstraint(result)(Symbol(operator))(arguments.map(toVariable(_, declaredVariables, cspom)): _*))
    }
  }

  @annotation.tailrec
  private def readerToString(r: Reader[Char], stb: StringBuilder): String =
    if (r.atEnd) {
      stb.toString
    } else {
      readerToString(r.rest, stb.append(r.first))
    }

  def parseTable(text: String, arity: Int, size: Int): Relation[Int] = {

    val traverse = new Traversable[String] {
      val st = new StringTokenizer(text, "|")
      def foreach[U](f: String => U): Unit = {
        while (st.hasMoreTokens()) {
          f(st.nextToken())
        }
      }
    }

    MDD[Int] {

      traverse
        .view
        .map { nt =>
          val t = nt.trim.split(" +").toList.filter(_.nonEmpty)
          assert(t.isEmpty || t.length == arity, s"${t.toSeq} should have length $arity, has length ${t.length}, empty = ${t.isEmpty}")
          t.map(_.toInt)
        }
        .filter(_.nonEmpty)

    }

    //    val st = new StringTokenizer(text, "|")
    //
    //    var table = MDD.empty[Int]
    //
    //    while (st.hasMoreTokens) {
    //      val nt = st.nextToken()
    //      println(nt)
    //      val t = nt.trim.split(" +")
    //      require(t.length == arity, s"${t.toSeq} should have length $arity")
    //      table += t.map(_.toInt)
    //    }
    //
    //    table.reduce
  }

}
