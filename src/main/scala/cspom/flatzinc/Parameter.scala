package cspom.flatzinc

/**
 * classe abstraite représentant un paramètre de rpédicat
 */
abstract class Parameter {
  /**
   * @return transcription xml
   */
  def toXML: xml.Elem
  /**
   * @return transcription xml abrégée
   */
  def toShortXML: String
}
/**
 * Représentation d'un variable entier en paramètre
 * @param name nmo de la variable paramètre
 */
case class IntParameter(name: String) extends Parameter {

  def toXML: xml.Elem =
    <parameter name={ name } type="int"/>

  def toShortXML: String = {
    "int " + name
  }
}
object Parameter {

  /**
   * @param name nom du paramètre
   * @param type type du paramètre
   */
  def getParamater(name: String, `type`: String = "int"): Parameter = `type` match {
    case "int" => new IntParameter(name)
    case _ => throw new IllegalArgumentException("Type muse be int")
  }

  /**
   * @param pList list de paramèters
   * @param p paramètre à ajouter
   * @return p :: pList
   */
  def addParamater(pList: List[Parameter], p: Parameter): List[Parameter] = {
    p :: pList
  }

  /**
   * @param pList liste de paramètre
   * @return transcription xml de la liste de paramètre
   */
  def toXML(pList: List[Parameter]): xml.Elem = {
    <parameters>{ for { p <- pList } yield p.toXML + " " }</parameters>
  }

  /**
   * @param pList liste de paramètre
   * @return transcription xml abrégé de la liste de paramètre
   */
  def toShortXML(pList: List[Parameter]): String = {
    val str = "[" + pList.head.toShortXML
    for { p <- pList.tail } yield str + " |" + p.toShortXML
    str + "]"
  }
}