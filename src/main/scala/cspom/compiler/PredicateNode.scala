package cspom.compiler;

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

  def isInteger = operator.map(ConstraintParser.isInt).getOrElse(false)

  def isIdentifier = operator.map(ConstraintParser.isId).getOrElse(false)

  def isLeaf = {
    assume(child.nonEmpty || isInteger || isIdentifier,
      "Leaves should be variables or constants, was " + this);

    child.isEmpty
  }

  def tree(stb: StringBuilder, level: Int) {
    for (i <- 1 to level) {
      stb.append("-");
    }
    stb.append(operator);
    for (p <- parameters) {
      stb.append('{').append(p).append('}');
    }
    stb.append('\n');
    for (c <- child) {
      c.tree(stb, level + 1)
    }
    for (s <- sibling) {
      s.tree(stb, level);
    }
  }

  def siblings = new Iterator[PredicateNode] {
    var current: Option[PredicateNode] = Some(PredicateNode.this)

    override def hasNext = current.isDefined

    override def next = {
      val ret = current.get;
      current = ret.sibling;
      ret
    }
  }
}
