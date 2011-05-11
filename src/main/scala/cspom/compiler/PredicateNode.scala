package cspom.compiler;
import scala.collection.mutable.Stack
import java.util.Deque;
import java.util.LinkedList;
import java.util.StringTokenizer;
import java.util.regex.Pattern;

final class PredicateNode {
  var sibling: PredicateNode = null;
  var child: PredicateNode = null;
  var operator: String = null;
  var parameters: String = null;

  override def toString = {
    val stb = new StringBuilder();
    tree(stb, 0);
    stb.toString();
  }

  def isInteger() = {
    if (operator == null) {
      false;
    } else {
      PredicateScanner.INTEGER.matcher(operator).matches();
    }
  }

  def isIdentifier() = {
    if (operator == null) {
      false;
    } else {
      PredicateScanner.IDENTIFIER.matcher(operator).matches();
    }
  }

  def isLeaf() = {
    if (child == null) {
      assume(isInteger || isIdentifier,
        "Leaves should be variables or constants, was " + this);

      true;
    } else {
      false;
    }
  }

  def tree(stb: StringBuilder, level: Int) {
    for (i <- 1 to level) {
      stb.append("-");
    }
    stb.append(operator);
    if (parameters != null) {
      stb.append('{').append(parameters).append('}');
    }
    stb.append('\n');
    if (child != null) {
      child.tree(stb, level + 1);
    }
    if (sibling != null) {
      sibling.tree(stb, level);
    }
  }
  
  def siblings = new Iterator[PredicateNode] {
    var current = PredicateNode.this

    override def hasNext = current != null

    override def next = {
      val ret = current;
      current = current.sibling;
      ret
    }
  }
}
