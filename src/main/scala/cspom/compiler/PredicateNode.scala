package cspom.compiler;
import scala.collection.mutable.Stack
import java.util.Deque;
import java.util.LinkedList;
import java.util.StringTokenizer;
import java.util.regex.Pattern;

final class PredicateNode {
  var sibling: Option[PredicateNode] = None;
  var child: Option[PredicateNode] = None;
  var operator: Option[String] = None;
  var parameters: Option[String] = None;

  override def toString = {
    val stb = new StringBuilder();
    tree(stb, 0);
    stb.toString();
  }

  def isInteger = operator match {
    case None => false
    case Some(o) => PredicateScanner.INTEGER.matcher(o).matches
  }

  def isIdentifier = operator match {
    case None => false
    case Some(o) => PredicateScanner.IDENTIFIER.matcher(o).matches
  }

  def isLeaf() = {
    if (child.isEmpty) {
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
    if (parameters.isDefined) {
      stb.append('{').append(parameters.get).append('}');
    }
    stb.append('\n');
    if (child.isDefined) {
      child.get.tree(stb, level + 1);
    }
    if (sibling.isDefined) {
      sibling.get.tree(stb, level);
    }
  }

  def siblings = new Iterator[PredicateNode] {
    var current: Option[PredicateNode] = Some(PredicateNode.this)

    override def hasNext = current.isDefined

    override def next = {
      val ret = current.get;
      current = current.get.sibling;
      ret
    }
  }
}
