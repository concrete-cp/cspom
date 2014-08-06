package cspom

import scala.reflect.ClassTag

trait Parameterized {

  def params: Map[String, Any]

  def getParam[A](name: String, typ: Class[A]): Option[A] =
    params.get(name).map(typ.cast)

  def getSeqParam[A](name: String, typ: Class[A]): Seq[A] = {
    params.get(name) match {
      case None => Seq()
      case Some(s: Seq[_]) => s.map(typ.cast)
      case _ => throw new IllegalArgumentException
    }
  }

  def getParamOrElse[A: ClassTag](name: String, default: => A): A = {
    params.get(name).map(_.asInstanceOf[A]).getOrElse(default)
  }

  def displayParams: String = params
    .map {
      case (k, Unit) => k
      case (k, v) => s"$k: $v"
    }
    .map {
      s => s" :: $s"
    }
    .mkString

  def hasParam = params.contains(_)
}