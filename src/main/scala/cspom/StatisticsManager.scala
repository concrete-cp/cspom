package cspom;

import scala.collection.mutable.MultiMap
import scala.collection.mutable.HashMap
import java.lang.reflect.Modifier
import java.lang.reflect.Field
import scala.annotation.tailrec
import com.typesafe.scalalogging.LazyLogging
import scala.util.Try
import scala.util.Failure
import org.scalameter._
import scala.reflect.runtime.universe._

class StatisticsManager extends LazyLogging {

  var objects: Map[String, AnyRef] = Map.empty

  def register(name: String, o: AnyRef) {
    require(!o.isInstanceOf[Class[_]])
    if (objects.contains(name)) {
      logger.warn(name + ": an object with the same name is already registered");
    }

    if (fields(o.getClass()).isEmpty) {
      logger.info(s"$o does not contain any statistic field")
    }

    objects += name -> o
  }

  private def annoted(f: Field) = f.getAnnotation(classOf[cspom.Statistic]) != null

  def tagged[T: TypeTag](name: String): T = get(name).map {
    case t => t.asInstanceOf[T]
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
          case v                     => Map(f.getName -> v)
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

  def averageBigInt(s: Seq[BigInt]): BigInt = {
    s.sum / s.size
  }

  def geom[A](s: Seq[A])(implicit n: Numeric[A]): Double = {
    import Numeric._
    val p = s.iterator.map(i => math.log(n.toDouble(i))).sum
    val d = p / s.length
    math.exp(d)

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

  def stDevBigInt(s: Seq[BigInt]): BigInt = {
    val avg = averageBigInt(s)
    val variance = s.map(i => (i - avg).pow(2)).sum / s.size
    util.Math.sqrt(variance)
  }
  
  

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

  def measureTry[A, T, U](f: => Try[A], measureBuilder: MeasureBuilder[T, U] = org.scalameter.`package`): (Try[A], Quantity[U]) = {
    var r: Try[A] = Failure(new IllegalStateException("No execution"))
    val t = measureBuilder.measure {
      r = f
      r.isSuccess
    }
    (r, t)

    //    var t = -System.nanoTime
    //    val r: Try[A] = f //.apply()
    //    t += System.nanoTime
    //    (r, t / 1e9)
  }

  def measure[A, T, U](f: => A, measureBuilder: MeasureBuilder[T, U] = org.scalameter.`package`): (Try[A], Quantity[U]) =
    measureTry(Try(f), measureBuilder)

}
