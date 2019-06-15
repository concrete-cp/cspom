package cspom

import scala.reflect.runtime.universe._

trait Parameterized {

  def params: Map[String, Any]

  def getParam[A: TypeTag](name: String): Option[A] = params.get(name).map(_.asInstanceOf[A])

  def getSeqParam[A: TypeTag](name: String): Seq[A] = {
    params.get(name).toSeq.flatMap {
      case s: Iterable[_] => s.map(_.asInstanceOf[A])
      case o => throw new IllegalArgumentException(s"$o is not a sequence")
    }
  }

  def getParamOrElse[A: TypeTag](name: String, default: => A): A = {
    getParam[A](name).getOrElse(default)
  }

  def displayParams: String = {
    params
      .map {
        case (k, ()) => k
        case (k, v) => s"$k: $v"
      }
      .map(s => s" :: $s")
      .mkString
  }

  def hasParam = params.contains(_)
}

object Annotations {
  def apply(params: (String, Any)*) = new Annotations(params.toMap)
}

class Annotations(val params: Map[String, Any]) extends Parameterized {
  def +(entry: (String, Any)) = new Annotations(params + entry)
}

case class WithParam[+T](obj: T, params: Map[String, Any] = Map()) extends Parameterized {
  def withParam(p: (String, Any)*): WithParam[T] = copy(params = params ++ p)
}
