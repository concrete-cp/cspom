package cspom

trait Parameterized {

  def params: Map[String, Any]

  def getParam[A](name: String, typ: Class[A]): Option[A] =
    try {
      params.get(name).map(typ.cast)
    } catch {
      case e: ClassCastException =>
        throw new IllegalArgumentException("Could not cast " + params(name) + ": " + params(name).getClass + " to " + typ)
    }

  def getSeqParam[A](name: String, typ: Class[A]): Seq[A] = {
    try {
      params.get(name) match {
        case None => Seq()
        case Some(s: Seq[_]) => s.map(typ.cast)
        case _ => throw new IllegalArgumentException
      }
    }
  }

  def displayParams: String =
    params map {
      case (k, Unit) => k
      case (k, v) => s"$k: $v"
    } map {
      s => s" :: $s"
    } mkString

  def hasParam = params.contains(_)
}