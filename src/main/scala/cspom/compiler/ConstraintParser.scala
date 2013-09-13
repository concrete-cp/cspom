package cspom.compiler;

import java.util.LinkedList
import java.util.List
import scala.collection.JavaConversions
import cspom.constraint.FunctionalConstraint
import cspom.constraint.GeneralConstraint
import cspom.variable.CSPOMVariable
import cspom.CSPOM
import scala.collection.mutable.Stack
import cspom.constraint.Predicate
import scala.collection.mutable.Stack
import java.util.Deque;
import java.util.LinkedList;
import java.util.StringTokenizer;
import java.util.regex.Pattern;

final object ConstraintParser {

  private val INTEGER = Pattern.compile("-?[0-9]*");

  private val IDENTIFIER = Pattern.compile("[a-zA-Z_]\\w*");

  def isInt(t: String) = INTEGER.matcher(t).matches

  def isId(t: String) = IDENTIFIER.matcher(t).matches

  def split(expression: String, problem: CSPOM) = {
    val root = scan(expression);

    require(!root.isLeaf, "Constraint expected");

    root.parameters.map { p =>
      problem.ctr(root.operator, p,
        root.child.toList.flatMap(_.siblings).map(addToProblem(_, problem)): _*)
    } getOrElse {
      problem.ctr(root.operator, root.child.toList.flatMap(_.siblings).map(addToProblem(_, problem)): _*)
    }
  }

  private def addToProblem(node: PredicateNode, problem: CSPOM): CSPOMVariable = {
    if (node.isLeaf) {
      problem.variable(node.operator).getOrElse {
        assume(node.isInteger, s"$node is not a valid leaf")
        problem.varOf(node.operator.toInt)
      }
    } else {
      node.parameters.map { p =>
        problem.is(node.operator, p, node.child.toList.flatMap(_.siblings).map(addToProblem(_, problem)): _*)
      } getOrElse {
        problem.is(node.operator, node.child.toList.flatMap(_.siblings).map(addToProblem(_, problem)): _*)
      }
    }
  }

  def scan(expression: String): PredicateNode = {
    val st = new StringTokenizer(expression, " {}(),", true);
    val stack = new Stack[PredicateNode]();
    var currentNode = new PredicateNode();
    var parameters: StringBuilder = null;

    while (st.hasMoreElements()) {
      val token = st.nextToken();
      if ("}" == token) {
        currentNode.parameters = Some(parameters.toString)
        parameters = null;
      } else if (parameters != null) {
        parameters.append(token);
      } else {
        token match {
          case " " =>
          case "{" =>
            require(currentNode.operatorIsDefined, "Empty operator");
            parameters = new StringBuilder();
          case "(" =>
            require(currentNode.operatorIsDefined, "Empty operator");
            val newPredicateNode = new PredicateNode();
            currentNode.child = Some(newPredicateNode)
            stack.push(currentNode);
            currentNode = newPredicateNode;
          case ")" =>
            require(!stack.isEmpty, "Too many )s");
            currentNode = stack.pop();
          case "," =>
            require(currentNode.operatorIsDefined, "Empty argument");
            val newPredicateNode = new PredicateNode();
            currentNode.sibling = Some(newPredicateNode)
            currentNode = newPredicateNode;
          case _ =>
            require(!currentNode.operatorIsDefined, "Delimiter expected in "
              + currentNode + " (" + expression + ")");
            currentNode.operator = token
        }
      }
    }
    currentNode;
  }

}
