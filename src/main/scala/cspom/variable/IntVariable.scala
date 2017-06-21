package cspom.variable

import com.typesafe.scalalogging.LazyLogging
import cspom.UNSATException
import cspom.util._

final class IntVariable(val domain: RangeSet[Infinitable])
  extends CSPOMVariable[Int] with LazyLogging {

  val asSortedSet = new ContiguousIntRangeSet(domain)

  if (domain.isEmpty) throw new UNSATException("A variable with empty domain was created")

  def isConvex = domain.isConvex

  if (asSortedSet.singletonMatch.isDefined) {
    logger.info(s"$domain: a variable domain should be of size 2 or more, created in ${Thread.currentThread().getStackTrace.toSeq}")
  }

  override def toString = domain.toString

  def contains[S >: Int](that: S): Boolean = that match {
    case true => contains(1)
    case false => contains(0)
    case t: Int => domain.intersects(IntInterval.singleton(t))
    case t: Long if t.isValidInt => contains(t.toInt)
    case _ => false
  }

  def isEmpty = false

  def intersected(that: SimpleExpression[_ >: Int]): SimpleExpression[Int] =
    that match {
      case const: CSPOMConstant[_] => const.intersected(this).asInstanceOf[CSPOMConstant[Int]]
      //      if domain.contains(Finite(c)) =>
      //        const.asInstanceOf[CSPOMConstant[Int]]
      //      //CSPOMConstant(c, Map("intersection" -> ((this, c))))
      case v: IntVariable => {
        val d = domain & v.domain
        if (d.isEmpty) {
          EmptyVariable
        } else if (domain == d) {
          this
        } else if (v.domain == d) {
          v
        } else {
          new ContiguousIntRangeSet(d).singletonMatch match {
            case Some(s) => CSPOMConstant(s) //, Map("intersection" -> ((this, v))))
            case None => new IntVariable(d) //, Map("intersection" -> ((this, v))))
          }
        }
      }
      case v: FreeVariable => this //new IntVariable(domain, Map("intersection" -> ((this, v))))
      case b: BoolVariable => {
        IntVariable(0, 1) intersected this
      }
      case t =>
        throw new IllegalArgumentException("Cannot intersect " + this + " with " + t)
    }

  def fullyDefined = domain.fullyDefined

  def searchSpace = domain.ranges.iterator.foldLeft(0.0) {
    case (acc, itv) => acc + (itv.itvSize match {
      case Finite(f) => f.toDouble
      case PlusInf => Double.PositiveInfinity
      case MinInf => throw new AssertionError()
    })
  }
}

object IntVariable {
  def apply(values: Range): IntVariable = apply(
    IntInterval(values.head, values.last))

  def apply(values: Interval[Infinitable]): IntVariable = apply(RangeSet(values))

  def apply(values: RangeSet[Infinitable]): IntVariable = new IntVariable(values)

  def apply(values: Int*): IntVariable = ofSeq(values)

  def ofSeq(values: Seq[Int]): IntVariable = {
    new IntVariable(RangeSet(values.map(
      v => IntInterval.singleton(v))))
  }

  def free(): IntVariable =
    new IntVariable(RangeSet.allInt)

  def unapply(v: IntVariable) = Some(v.domain)

}
