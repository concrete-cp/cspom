package cspom.variable
import scala.collection.JavaConversions

trait IntDomain extends Seq[Int] {
  //def values: Seq[Int]
  def getValues = JavaConversions.asJavaCollection(this)
  def intersect(domain: IntDomain): IntDomain
  def toXCSP: String
}

object IntDomain {
  def of(values: Int*) = {
    require(values.take(2).size > 1, "constants not accepted, use appropriate constructor")
    values match {
      case r: Range => new IntInterval(r.head, r.last)
      case s: Seq[Int] if (values.last - values.head == values.size - 1) => new IntInterval(s.head, s.last)
      case s => new IntSeq(values)
    }
  }

  /**
   * Parse the given domain given as a String. Domains are usually sequence of
   * values separated by spaces such as "1 3 -4 5" or intervals in the format
   * "a..b". Sequences of values and intervals such as "1 3..10 18..30" are
   * allowed and converted to a sequence of values.
   *
   * @param domain
   *            The String domain to parse
   * @return The resulting Domain object
   */
  def valueOf(desc: String) = {
    val values = desc.trim.split(" +").flatMap { v =>
      if (v.contains("..")) {
        IntInterval.valueOf(v);
      } else {
        Seq(v.trim.toInt);
      }
    }

    of(values: _*)
    //        //        if (values.size == 1) {
    //        //          new Constant(values.head)
    //        //        } else 
    //        if (values == (values.head to values.last)) {
    //          new IntInterval(values.head, values.last)
    //        } else {
    //          new ExtensiveDomain(values)
    //        }

  }

}