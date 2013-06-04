package cspom.compiler;

final class PredicateNode {
  var sibling: Option[PredicateNode] = None;
  var child: Option[PredicateNode] = None;
  private var _operator: Option[String] = None;
  var parameters: Option[String] = None;

  override def toString = tree(new StringBuilder()).toString;

  def operator = _operator.get

  def operator_=(v: String) {
    require(_operator.isEmpty)
    _operator = Some(v)
  }

  def operatorIsDefined = _operator.nonEmpty

  def isInteger = ConstraintParser.isInt(operator)

  def isIdentifier = ConstraintParser.isId(operator)

  def isLeaf = {
    assume(child.nonEmpty || isInteger || isIdentifier,
      "Leaves should be variables or constants, was " + this);

    child.isEmpty
  }

  def tree(stb: StringBuilder): StringBuilder = {
    stb.append(operator);
    for (p <- parameters) {
      stb.append('{').append(p).append('}');
    }
    stb.append('\n');
    for (c <- child) {
      stb.append("(")
      c.tree(stb)
      stb.append(")")
    }
    for (s <- sibling) {
      stb.append(", ")
      s.tree(stb);
    }
    stb
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
