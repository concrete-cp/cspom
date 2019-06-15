package cspom

import java.lang.reflect.Field

import com.typesafe.scalalogging.LazyLogging

import scala.annotation.tailrec
import scala.reflect.runtime.universe._

class StatisticsManager extends LazyLogging {

  var objects: Map[String, AnyRef] = Map.empty

  def register(name: String, o: AnyRef): Unit = {
    require(!o.isInstanceOf[Class[_]])
    if (objects.contains(name)) {
      logger.warn(name + ": an object with the same name is already registered");
    }

    if (fields(o.getClass).isEmpty) {
      logger.info(s"$o does not contain any statistic field")
    }

    objects += name -> o
  }

  def tagged[T: TypeTag](name: String): T = get(name).map {
    case t: T => t
  }
    .getOrElse {
      throw new NoSuchElementException(name + " not in " + objects.keys.toString)
    }

  def apply(name: String): AnyRef = tagged(name)

  def get(name: String): Option[AnyRef] = {

    val fieldNameAt = name.lastIndexOf('.')
    objects
      .get(name.substring(0, fieldNameAt))
      .flatMap { obj =>
        val fieldName = name.substring(fieldNameAt + 1, name.length)
        fields(obj.getClass)
          .find(f => f.getName == fieldName)
          .map { f =>
            f.setAccessible(true)
            f.get(obj)
          }
      }
  }

  def digest: Map[String, Any] = objects flatMap {
    case (s, o) =>
      fields(o.getClass).flatMap { f =>
        f.setAccessible(true)
        val map = f.get(o) match {
          case sm: StatisticsManager => sm.digest
          case v => Map(f.getName -> v)
        }
        map.map { case (k, v) => s"$s.$k" -> v }
      }
  }

  override def toString: String = digest.map(t => s"${t._1} = ${t._2}").toSeq.sorted.mkString("\n")

  def reset(): Unit = {
    objects = Map.empty
  }

  private def annoted(f: Field) = f.getAnnotation(classOf[cspom.Statistic]) != null

  private def fields(c: Class[_], f: List[Field] = Nil): List[Field] =
    if (c == null) {
      f
    } else {
      fields(c.getSuperclass, c.getDeclaredFields.toList.filter(annoted) ::: f)
    }
}

object StatisticsManager {

  def average[A: Numeric](s: Iterable[A]): Double = average(s.iterator)

  def average[A](xs: Iterator[A])(implicit n: Numeric[A]): Double = {
    if (xs.hasNext) {
      var m = n.toDouble(xs.next)
      var k = 1
      for (x <- xs) {
        k += 1
        m += (n.toDouble(x) - m) / k
      }
      m
    } else {
      Double.NaN
    }
  }

  def stDev[A: Numeric](s: Iterable[A]): Double = stDev(s.iterator)

  def stDev[A: Numeric](s: Iterator[A]): Double = math.sqrt(variance(s))

  def variance[A](xs: Iterator[A])(implicit n: Numeric[A]): Double = {
    if (xs.hasNext) {
      var m = n.toDouble(xs.next)
      var s = 0.0
      var k = 1
      for (x <- xs map n.toDouble) {
        k += 1
        val mk = m + (x - m) / k
        s += (x - m) * (x - mk)
        m = mk
      }
      s / (k - 1)
    } else {
      Double.NaN
    }
  }

  def stDevBigInt(s: Seq[BigInt]): BigInt = {
    val avg = averageBigInt(s)
    val variance = s.map(i => (i - avg).pow(2)).sum / s.size
    util.Math.sqrt(variance)
  }

  def averageBigInt(s: Iterable[BigInt]): BigInt = {
    s.sum / s.size
  }

  def min[A: Ordering](s: Iterable[A]): A = s.min

  def max[A: Ordering](s: Iterable[A]): A = s.max

  @tailrec
  def findKMedian[A](arr: Seq[A], k: Int)(implicit o: Ordering[A]): A = {
    val pivot = arr(scala.util.Random.nextInt(arr.size))
    val (s, b) = arr partition (o.gt(pivot, _))
    s.size match {
      case `k` => pivot

      case 0 => // Used to avoid infinite repetition
        val (s, b) = arr.partition(pivot == _)
        if (s.lengthCompare(k) > 0) {
          pivot
        } else {
          findKMedian(b, k - s.size)
        }

      case i if i < k => findKMedian(b, k - s.size)

      case _ => findKMedian(s, k)

    }
  }

  def median[A: Ordering](arr: Seq[A]): A = {
    if (arr.isEmpty) {
      throw new NoSuchElementException("Median of empty sequence")
    } else {
      findKMedian(arr, arr.size / 2)
    }
  }

  def fq[A: Ordering](arr: Seq[A]): A = {
    if (arr.isEmpty) {
      throw new NoSuchElementException("Median of empty sequence")
    } else {
      findKMedian(arr, arr.size / 4)
    }
  }

  def tq[A: Ordering](arr: Seq[A]): A = {
    if (arr.isEmpty) {
      throw new NoSuchElementException("Median of empty sequence")
    } else {
      findKMedian(arr, 3 * arr.size / 4)
    }
  }

  def measure[A](f: => A): (scala.util.Try[A], Double) = measureTry(scala.util.Try(f))

  def measureTry[A](f: => scala.util.Try[A]): (scala.util.Try[A], Double) = {
    var t = -System.nanoTime
    val r: scala.util.Try[A] = f
    t += System.nanoTime
    (r, t / 1e6)
  }

}
