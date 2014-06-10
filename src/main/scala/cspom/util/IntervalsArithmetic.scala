package cspom.util

import java.math.RoundingMode
import java.math.RoundingMode
import com.google.common.math.IntMath
import com.google.common.collect.DiscreteDomain
import com.google.common.collect.Cut
import IntRangeSet._
import scala.math.Ordering.IntOrdering

object IntervalsArithmetic {

  def apply(
    f: (IntInterval, IntInterval) => IntInterval,
    ii: IntRangeSet, jj: IntRangeSet): IntRangeSet = {
    var result = IntRangeSet()
    for (i <- ii.ranges; j <- jj.ranges) yield {
      result ++= f(i, j)
    }
    result
  }

  def apply(f: IntInterval => IntInterval, ii: IntRangeSet): IntRangeSet = {
    ii.ranges.foldLeft(IntRangeSet())(_ ++ f(_))
  }

  private def asInfinities(r: IntInterval): (Infinitable, Infinitable) = {
    val l = if (r.hasLowerBound) {
      Finite(r.lowerEndpoint)
    } else {
      MinInf
    }

    val u = if (r.hasUpperBound) {
      Finite(r.upperEndpoint)
    } else {
      PlusInf
    }

    (l, u)
  }

  private def asRange(l: Infinitable, lbt: BoundType, u: Infinitable, ubt: BoundType) = {
    (l, u) match {
      case (Finite(l), Finite(u)) => {
        if (l > u) {
          // Empty interval
          IntInterval.closedOpen(l, l)
        } else {
          IntInterval(l, lbt, u, ubt)
        }
      }
      case (MinInf, Finite(u)) => IntInterval.upTo(u, ubt)
      case (Finite(l), PlusInf) => IntInterval.downTo(l, lbt)
      case (MinInf, PlusInf) => IntInterval.all
      case (l, u) => throw new IllegalArgumentException(s"Incoherent interval ($l, $u)")
    }
  }

  private def minBound[A](b: (A, BoundType)*)(implicit ord: Ordering[A]): (A, BoundType) = {
    b.min(Ordering.Tuple2(ord, BoundType.closedIsLess))
  }

  private def maxBound[A](b: (A, BoundType)*)(implicit ord: Ordering[A]): (A, BoundType) = {
    b.max(Ordering.Tuple2(ord, BoundType.closedIsMore))
  }

  implicit class RangeArithmetics(val r: IntRangeSet) extends AnyVal {
    def +(i: IntRangeSet): IntRangeSet = IntervalsArithmetic(_ + _, r, i)
    def unary_-(): IntRangeSet = IntervalsArithmetic(-_, r)
    def -(i: IntRangeSet): IntRangeSet = IntervalsArithmetic(_ - _, r, i)
    def *(i: IntRangeSet): IntRangeSet = IntervalsArithmetic(_ * _, r, i)
    def /(i: IntRangeSet): IntRangeSet = IntervalsArithmetic(_ / _, r, i)
    def abs: IntRangeSet = IntervalsArithmetic(_.abs, r)
  }

  implicit class LazyRangeArithmetics(val r: IntRangeSet) extends AnyVal {
    def ~+(i: IntRangeSet): IntRangeSet = IntervalsArithmetic(_ + _, r, i)
    def unary_~-(): IntRangeSet = IntervalsArithmetic(-_, r)
    def ~-(i: IntRangeSet): IntRangeSet = IntervalsArithmetic(_ - _, r, i)
    def ~*(i: IntRangeSet): IntRangeSet = IntervalsArithmetic(_ * _, r, i)
    def ~/(i: IntRangeSet): IntRangeSet = IntervalsArithmetic(_ / _, r, i)
    def labs: IntRangeSet = IntervalsArithmetic(_.abs, r)
  }

  implicit class Arithmetics(val r: IntInterval) extends AnyVal {
    /**
     * [a, b] + [c, d] = [a + c, b + d]
     * [a, b] − [c, d] = [a − d, b − c]
     * [a, b] × [c, d] = [min (a × c, a × d, b × c, b × d), max (a × c, a × d, b × c, b × d)]
     * [a, b] ÷ [c, d] = [min (a ÷ c, a ÷ d, b ÷ c, b ÷ d), max (a ÷ c, a ÷ d, b ÷ c, b ÷ d)] when 0 is not in [c, d].
     */

    def +(i: IntInterval): IntInterval = {
      val (a, bc) = asInfinities(r.canonical)
      val (c, dc) = asInfinities(i.canonical)

      val b = bc - Finite(1)
      val d = dc - Finite(1)

      val lb = a + c

      val ub = b + d

      asRange(lb, Closed, ub, Closed)
    }

    def unary_-(): IntInterval = {
      val (a, b) = asInfinities(r)

      asRange(-b, r.upperBoundType, -a, r.lowerBoundType)
    }

    def -(i: IntInterval) = this + -i

    def -(v: Int) = this + -v

    def *(i: IntInterval): IntInterval = {

      val (a, bc) = asInfinities(r.canonical)
      val (c, dc) = asInfinities(i.canonical)

      val d = dc - Finite(1)
      val b = bc - Finite(1)

      val l = List(a * c, a * d, b * c, b * d).min

      val u = List(a * c, a * d, b * c, b * d).max

      asRange(l, Closed, u, Closed)

    }

    def /(i: IntInterval): IntInterval = {
      if (i.contains(0)) {
        IntInterval.all
      } else {
        val (a, bc) = asInfinities(r.canonical)
        val (c, dc) = asInfinities(i.canonical)

        val d = dc - Finite(1)
        val b = bc - Finite(1)

        val l = Seq(
          a.div(c, RoundingMode.CEILING),
          a.div(d, RoundingMode.CEILING),
          b.div(c, RoundingMode.CEILING),
          b.div(d, RoundingMode.CEILING)).min

        val u = Seq(
          a.div(c, RoundingMode.FLOOR),
          a.div(d, RoundingMode.FLOOR),
          b.div(c, RoundingMode.FLOOR),
          b.div(d, RoundingMode.FLOOR)).max

        asRange(l, Closed, u, Closed)
      }
    }

    def /(v: Int): IntInterval = {
      if (v == 0) {
        IntInterval.all
      } else {
        val (lb, ub) = asInfinities(r)
        val d = Finite(v)
        if (v > 0) {
          asRange(
            lb.div(d, RoundingMode.CEILING), if (lb.divisible(d)) r.lowerBoundType else Closed,
            ub.div(d, RoundingMode.FLOOR), if (ub.divisible(d)) r.upperBoundType else Closed)
        } else {
          asRange(
            ub.div(d, RoundingMode.CEILING), if (ub.divisible(d)) r.upperBoundType else Closed,
            lb.div(d, RoundingMode.FLOOR), if (lb.divisible(d)) r.lowerBoundType else Closed)
        }
      }
    }

    def abs: IntInterval = {
      val (l, u) = asInfinities(r)
      if (u <= Finite(0)) { -r }
      else if (l >= Finite(0)) { r }
      else {
        val (b, bt) = maxBound[Infinitable]((-l, r.lowerBoundType), (u, r.upperBoundType))

        asRange(Finite(0), Closed, b, bt)
      }
    }

  }
}
