package cspom.xcsp

import scala.annotation.elidable.ASSERTION
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharSequenceReader
import cspom.CSPOM
import cspom.variable.CSPOMVariable
import cspom.variable.CSPOMExpression
import cspom.variable.IntConstant

sealed trait PredicateNode

final case class PredicateConstraint(val operator: String, val arguments: Seq[PredicateNode]) extends PredicateNode
final case class PredicateConstant(val constant: Int) extends PredicateNode
final case class PredicateVariable(val variableId: String) extends PredicateNode

//final class PredicateNode(val operator: String, val child: Seq[PredicateNode]) {
//
//  def isLeaf = child.isEmpty
//
//  def tree(stb: StringBuilder): StringBuilder = {
//    stb.append(operator);
//    //    for (p <- parameters) {
//    //      stb.append('{').append(p).append('}');
//    //    }
//    stb.append('\n');
//    for (c <- child) {
//      stb.append("(")
//      c.tree(stb)
//      stb.append(")")
//    }
//    stb
//  }
//
//}

final object ConstraintParser extends JavaTokenParsers {

  def integer = wholeNumber ^^ (_.toInt)

  def func: Parser[PredicateNode] =
    ident ~ ("(" ~> repsep(func, ",") <~ ")") ^^ {
      case ident ~ children => new PredicateConstraint(ident, children)
    } |
      ident ^^ (new PredicateVariable(_)) |
      integer ^^ (new PredicateConstant(_))

  def mapFunc(map: Map[String, String]): Parser[String] =
    ident ~ ("(" ~> repsep(mapFunc(map), ",") <~ ")") ^^ {
      case ident ~ children => ident + children.mkString("(", ", ", ")")
    } |
      ident ^^ (map(_)) |
      integer ^^ (_.toString)

  def split(expression: String, problem: CSPOM) = {
    func(new CharSequenceReader(expression)).get match {
      case PredicateConstraint(operator, arguments) => problem.ctr(operator, arguments.map(addToProblem(_, problem)): _*)
      case _ => throw new IllegalArgumentException("Constraint expected")
    }

  }

  private def addToProblem(node: PredicateNode, problem: CSPOM): CSPOMExpression = {
    node match {
      case PredicateConstant(value) => IntConstant(value)
      case PredicateVariable(variableId) => problem.variable(variableId).get
      case PredicateConstraint(operator, arguments) => problem.is(operator, arguments.map(addToProblem(_, problem)): _*)
    }
  }

}
