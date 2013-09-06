package cspom.variable

final class IntVariable(name: String, domain: IntDomain, params: String*)
  extends CSPOMVariable(name, params: _*) {

}

object IntIntervalVariable {
  def valueOf(interval: String) = interval.trim().split("\\.\\.") match {
    case Array(a, b) => new IntInterval(a.toInt, b.toInt)
    case _ => throw new NumberFormatException("Interval format must be a..b");
  }

}