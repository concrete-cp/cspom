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

sealed trait PredicateNode

final case class PredicateConstraint(val operator: String, val arguments: Seq[PredicateNode]) extends PredicateNode {
  require(arguments.nonEmpty)
}
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

  private def integer = wholeNumber ^^ (_.toInt)

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

  def split(expression: String, declaredVariables: Map[String, IntVariable]): (Seq[CSPOMVariable[_]], Seq[CSPOMConstraint[_]]) = {
    func(new CharSequenceReader(expression)).get match {
      case PredicateConstraint(operator, arguments) =>
        val (sub, genVars, genCons) = arguments.map(toVariable(_, declaredVariables)).unzip3

        (genVars.flatten, genCons.flatten :+ CSPOMConstraint(Symbol(operator), sub: _*))

      case _ => throw new IllegalArgumentException("Constraint expected, was " + expression)
    }

  }

  private def toVariable(node: PredicateNode, declaredVariables: Map[String, IntVariable]): (CSPOMExpression[_], Seq[CSPOMVariable[_]], Seq[CSPOMConstraint[_]]) = {
    node match {
      case PredicateConstant(value) => (CSPOMConstant(value), Seq(), Seq())
      case PredicateVariable(variableId) => (declaredVariables(variableId), Seq(), Seq())
      case PredicateConstraint(operator, arguments) => {
        val result = CSPOMVariable.aux()
        val (sub, genVars, genCons) = arguments.map(toVariable(_, declaredVariables)).unzip3
        (result, genVars.flatten :+ result, genCons.flatten :+ CSPOMConstraint(result, Symbol(operator), sub: _*))
      }
    }
  }

}
