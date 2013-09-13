package cspom.variable

final class IntSeq(val values: Seq[Int]) extends IntDomain {
  //  override def equals(obj: Any) = obj match {
  //    case ed: IntSeq => ed.values == values
  //    case _ => false
  //  }

  override def toString =
    if (size > 5)
      values.take(5).mkString("{", ", ", "...}")
    else
      values.mkString("{", ", ", "}")

  def toXCSP = values.mkString(", ")

  def intersect(domain: IntDomain) =
    new IntSeq(this.values.intersect(domain))

  def iterator: Iterator[Int] = values.iterator

  def apply(idx: Int): Int = values(idx)
  def length: Int = values.length
}
