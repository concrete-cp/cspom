package cspom

import scala.reflect.runtime.universe._

trait Parameterized {

  def params: Map[String, Any]

  def getParam[A: TypeTag](name: String): Option[A] = params.get(name).map(_.asInstanceOf[A])

  def getSeqParam[A: TypeTag](name: String): Seq[A] = {
    params.get(name).toSeq.flatMap {
      case s: Seq[_] => s.map(_.asInstanceOf[A])
      case _         => throw new IllegalArgumentException
    }
  }

  def getParamOrElse[A: TypeTag](name: String, default: => A): A = {
    getParam[A](name).getOrElse(default)
  }

  def displayParams: String = {
    params
      .map {
        case (k, Unit) => k
        case (k, v)    => s"$k: $v"
      }
      .map(s => s" :: $s")
      .mkString
  }

  def hasParam = params.contains(_)
}