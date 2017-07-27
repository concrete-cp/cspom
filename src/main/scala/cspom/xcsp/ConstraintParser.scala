package cspom.xcsp

import java.util.StringTokenizer

import cspom.extension.{MDDRelation, Relation}
import cspom.variable.{CSPOMConstant, SimpleExpression}
import cspom.{CSPOM, CSPOMConstraint}

import scala.collection.mutable
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.input.CharSequenceReader

sealed trait PredicateNode

final case class PredicateConstraint(val operator: String, val arguments: Seq[PredicateNode]) extends PredicateNode {
  require(arguments.nonEmpty)
}

final case class PredicateConstant(val constant: Int) extends PredicateNode

final case class PredicateVariable(val variableId: String) extends PredicateNode

final object ConstraintParser extends JavaTokenParsers {

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

  def parseTable(text: String, arity: Int, size: Int): Relation[Int] = {

    val traverse = new Traversable[String] {
      val st = new StringTokenizer(text, "|")

      def foreach[U](f: String => U): Unit = {
        while (st.hasMoreTokens()) {
          f(st.nextToken())
        }
      }
    }

    val relation: mutable.Buffer[Array[Int]] = traverse
      .view
      .map(_.trim)
      .filter(_.nonEmpty)
      .map { nt =>
        val t: Array[Int] = nt.trim.split("\\s+").map(_.toInt)
        // println(t.mkString("[", " ", "]"))
        assert(t.isEmpty || t.length == arity, s"$t should have length $arity, has length ${t.length}, empty = ${t.isEmpty}")
        t
      }
      .toBuffer

    //println(relation)

    val array:Array[Array[Int]] = relation.toArray

    MDDRelation(array).reduce()

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

  private def integer = wholeNumber ^^ (_.toInt)

  private def toVariable(node: PredicateNode, declaredVariables: Map[String, SimpleExpression[Int]], cspom: CSPOM): SimpleExpression[_] = {
    node match {
      case PredicateConstant(value) => CSPOMConstant(value)
      case PredicateVariable(variableId) => declaredVariables(variableId)
      case PredicateConstraint(operator, arguments) => cspom.defineFree(result =>
        CSPOMConstraint(result)(Symbol(operator))(arguments.map(toVariable(_, declaredVariables, cspom)): _*))
    }
  }

}
