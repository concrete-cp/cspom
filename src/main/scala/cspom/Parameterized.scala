package cspom

import scala.reflect.ClassTag

trait Parameterized {

  def params: Map[String, Any]

  def getParam[A: ClassTag](name: String): Option[A] =
    params.get(name).map {
      case p: A => p
      case _    => throw new ClassCastException
    }

  def getSeqParam[A: ClassTag](name: String): Seq[A] = {
    params.get(name).toSeq.flatMap {
      case s: Seq[A] => s.map(_.asInstanceOf[A])
      case _         => throw new IllegalArgumentException
    }
  }

  def getParamOrElse[A: ClassTag](name: String, default: => A): A = {
    getParam(name).getOrElse(default)
  }

  def displayParams: String = {
    ???
    ""
    //    params
    //    .map {
    //        case (k, Unit) => k
    //        case (k, v) => s"$k: $v"
    //      }
    //      .map {
    //        s => s" :: $s"
    //      }
    //      .mkString
  }

  def hasParam = params.contains(_)
}