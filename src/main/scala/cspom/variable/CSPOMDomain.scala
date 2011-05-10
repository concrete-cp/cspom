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
    val listOfValues = desc.trim.split(" +")

    if (listOfValues.size == 1 && listOfValues(0).contains("..")) {
      IntInterval.valueOf(listOfValues(0));
    } else {
      var values: List[java.lang.Integer] = Nil

      for (currentValue <- listOfValues) {
        if (currentValue.contains("..")) {
          values ++= IntInterval.valueOf(currentValue).values;
        } else {
          values :+= java.lang.Integer.valueOf(currentValue.trim);
        }
      }

      new ExtensiveDomain(values)
    }

  }

}