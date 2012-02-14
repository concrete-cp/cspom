package cspom.compiler;

import scala.collection.mutable.Stack
import java.util.Deque;
import java.util.LinkedList;
import java.util.StringTokenizer;
import java.util.regex.Pattern;

final object PredicateScanner {

  val INTEGER = Pattern.compile("-?[0-9]*");

  val IDENTIFIER = Pattern.compile("[a-zA-Z_]\\w*");

  def scan(expression: String): PredicateNode = {
    val st = new StringTokenizer(expression, " {}(),", true);
    val stack = new Stack[PredicateNode];
    var currentNode = new PredicateNode;
    var parameters: StringBuilder = null;
    
    while (st.hasMoreElements()) {
      val token = st.nextToken();
      if ("}" == token) {
        currentNode.parameters = Some(parameters.toString)
        parameters = null;
      } else if (parameters != null) {
        parameters.append(token);
      } else token match {
        case " " =>
        case "{" =>
          assume(currentNode.operator.isDefined, "Empty operator");
          parameters = new StringBuilder();
        case "(" =>
          assume(currentNode.operator.isDefined, "Empty operator");
          val newPredicateNode = new PredicateNode();
          currentNode.child = Some(newPredicateNode)
          stack.push(currentNode);
          currentNode = newPredicateNode;
        case ")" =>
          assume(!stack.isEmpty, "Too many )s");
          currentNode = stack.pop();
        case "," =>
          assume(currentNode.operator.isDefined, "Empty argument");
          val newPredicateNode = new PredicateNode();
          currentNode.sibling = Some(newPredicateNode)
          currentNode = newPredicateNode;
        case _ =>
          assume(currentNode.operator.isEmpty, "Delimiter expected in "
            + currentNode + " (" + expression + ")");
          currentNode.operator = Some(token)
      }
    }
    currentNode;
  }

}
