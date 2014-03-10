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

  def displayParams: String = if (params.isEmpty) { "" } else { params.mkString(" :: ", " :: ", "") }
  
  def hasParam = params.contains(_)
}