package cspom.variable

trait CSPOMDomain[T] {
  def values: Seq[T]
  def size: Int = values.size
  def intersect(domain: CSPOMDomain[T]): CSPOMDomain[T]
}

object CSPOMDomain {
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
    desc.trim.split(" +") match {
      case Array(single) if single contains ".." =>
        IntInterval.valueOf(single)
      case listOfValues =>
        val values = (listOfValues.iterator.map { v =>
          if (v.contains("..")) {
            IntInterval.valueOf(v).values;
          } else {
            List(java.lang.Integer.valueOf(v.trim));
          }
        }).flatten.toSeq
        if (values == (Int.unbox(values.head) to values.last)) {
          new IntInterval(values.head, values.last)
        } else {
          new ExtensiveDomain(values)
        }

    }

  }

}