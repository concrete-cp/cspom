package cspom;

import scala.collection.mutable.MultiMap
import scala.collection.mutable.HashMap
import java.lang.reflect.Modifier
import java.lang.reflect.Field
import scala.annotation.tailrec
import com.typesafe.scalalogging.LazyLogging
import scala.util.Try

class StatisticsManager extends LazyLogging {

  var objects: Map[String, AnyRef] = Map.empty

  def register(name: String, o: AnyRef) {
    require(!o.isInstanceOf[Class[_]])
    if (objects.contains(name)) {
      logger.warn(name + ": an object with the same name is already registered");
    }

    if (fields(o.getClass()).isEmpty) {
      logger.warn(s"$o does not contain any statistic field")
    }

    objects += name -> o
  }

  private def annoted(f: Field) = f.getAnnotation(classOf[cspom.Statistic]) != null

  def apply(name: String): AnyRef = {

    val fieldNameAt = name.lastIndexOf('.')
    val obj = objects.get(name.substring(0, fieldNameAt)).get
    val fieldName = name.substring(fieldNameAt + 1, name.length)
    fields(obj.getClass).find(f => f.getName == fieldName) match {
      case Some(f) => { f.setAccessible(true); f.get(obj) }
      case None => throw new IllegalArgumentException(
        s"Could not find $name ($fieldName in ${fields(obj.getClass)})")
    }

  }

  private def fields(c: Class[_], f: List[Field] = Nil): List[Field] =
    if (c == null) {
      f
    } else {
      fields(c.getSuperclass, c.getDeclaredFields.toList.filter(annoted) ::: f)
    }

  def digest: Map[String, Any] = objects flatMap {
    case (s, o) =>
      fields(o.getClass).flatMap { f =>
        f.setAccessible(true)
        val map = f.get(o) match {
          case sm: StatisticsManager => sm.digest
          case v: AnyRef             => Map(f.getName -> v)
        }
        map.map { case (k, v) => s"$s.$k" -> v }
      }
  }

  override def toString = digest.map(t => t._1 + " = " + t._2).toSeq.sorted.mkString("\n")

  private def isIntType(input: Class[_]) = input == classOf[Int] || input == classOf[Long]

  private def isFloatType(input: Class[_]) = input == classOf[Float] || input == classOf[Double]

  def reset() {
    objects = Map.empty
  }
}

object StatisticsManager {

  def average[A: Numeric](s: Seq[A]): Double = average(s.iterator)
  def average[A](xs: Iterator[A])(implicit n: Numeric[A]): Double = {
    var m = n.toDouble(xs.next)
    var k = 1
    for (x <- xs) {
      k += 1
      m += (n.toDouble(x) - m) / k
    }
    m
  }

  def variance[A](xs: Iterator[A])(implicit n: Numeric[A]): Double = {
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
  }

  def stDev[A: Numeric](s: Iterator[A]): Double = math.sqrt(variance(s))

  def stDev[A: Numeric](s: Seq[A]): Double = stDev(s.iterator)

  def min[A: Ordering](s: Seq[A]): A = s.min

  def max[A: Ordering](s: Seq[A]): A = s.max

  @tailrec
  def findKMedian[A](arr: Seq[A], k: Int)(implicit o: Ordering[A]): A = {
    val pivot = arr(scala.util.Random.nextInt(arr.size))
    val (s, b) = arr partition (o.gt(pivot, _))
    if (s.size == k) {
      pivot
    } // The following test is used to avoid infinite repetition
    else if (s.isEmpty) {
      val (s, b) = arr.partition(pivot == _)
      if (s.size > k) {
        pivot
      } else {
        findKMedian(b, k - s.size)
      }
    } else if (s.size < k) {
      findKMedian(b, k - s.size)
    } else {
      findKMedian(s, k)
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

  def timeTry[A](f: => Try[A]): (Try[A], Double) = {
    var t = -System.nanoTime
    val r = Try(f).flatten
    t += System.nanoTime
    (r, t / 1e9)
  }

  def time[A](f: => A): (Try[A], Double) = timeTry(Try(f))

}

class TimedException(val time: Double, cause: Throwable) extends Exception(cause)
