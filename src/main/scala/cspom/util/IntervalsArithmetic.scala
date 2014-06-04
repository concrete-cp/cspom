package cspom.util

import java.math.RoundingMode
import java.math.RoundingMode
import com.google.common.math.IntMath
import com.google.common.collect.DiscreteDomain
import com.google.common.collect.Cut
import RangeSet._

object IntervalsArithmetic {

  def apply[A <% Ordered[A]](
    f: (Interval[A], Interval[A]) => Interval[A],
    ii: RangeSet[A], jj: RangeSet[A]): RangeSet[A] = {
    //var result = RangeSet[A]()
    val calcs = for (i <- ii.ranges; j <- jj.ranges) yield {
      f(i, j)
    }
    calcs.foldLeft(RangeSet[A]())(_ ++ _)
  }

  def apply[A <% Ordered[A]](f: Interval[A] => Interval[A], ii: RangeSet[A]): RangeSet[A] = {
    ii.ranges.foldLeft(RangeSet[A]())(_ ++ f(_))
  }

  private def asInfinities(r: Interval[Int]): (Infinitable, Infinitable) = {
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
        if (l > u) { Interval.closedOpen(l, l) }
        else { Interval(l, lbt, u, ubt) }
      }
      case (MinInf, Finite(u)) => Interval.upTo(u, ubt)
      case (Finite(l), PlusInf) => Interval.downTo(l, lbt)
      case (MinInf, PlusInf) => Interval.all[Int]
      case _ => throw new IllegalStateException
    }
  }

  private def minBound[A](b: (A, BoundType)*)(implicit ord: Ordering[A]): (A, BoundType) = {
    b.min(Ordering.Tuple2(ord, BoundType.closedIsLess))
  }

  private def maxBound[A](b: (A, BoundType)*)(implicit ord: Ordering[A]): (A, BoundType) = {
    b.max(Ordering.Tuple2(ord, BoundType.closedIsMore))
  }

  implicit class RangeArithmetics(val r: RangeSet[Int]) extends AnyVal {
    def +(i: RangeSet[Int]): RangeSet[Int] = IntervalsArithmetic(_ + _, r, i)
    def unary_-(): RangeSet[Int] = IntervalsArithmetic(-_, r)
    def -(i: RangeSet[Int]): RangeSet[Int] = IntervalsArithmetic(_ - _, r, i)
    def *(i: RangeSet[Int]): RangeSet[Int] = IntervalsArithmetic(_ * _, r, i)
    def /(i: RangeSet[Int]): RangeSet[Int] = IntervalsArithmetic(_ / _, r, i)
    def abs: RangeSet[Int] = IntervalsArithmetic(_.abs, r)
  }

  implicit class Arithmetics(val r: Interval[Int]) extends AnyVal {
    /**
     * [a, b] + [c, d] = [a + c, b + d]
     * [a, b] − [c, d] = [a − d, b − c]
     * [a, b] × [c, d] = [min (a × c, a × d, b × c, b × d), max (a × c, a × d, b × c, b × d)]
     * [a, b] ÷ [c, d] = [min (a ÷ c, a ÷ d, b ÷ c, b ÷ d), max (a ÷ c, a ÷ d, b ÷ c, b ÷ d)] when 0 is not in [c, d].
     */

    def +(i: Interval[Int]): Interval[Int] = {
      val (a, b) = asInfinities(r)
      val (c, d) = asInfinities(i)

      val lb = a + c
      val lbt = if (lb.isInfinity) { Open } else { r.lowerBoundType & i.lowerBoundType }

      val ub = b + d
      val ubt = if (ub.isInfinity) { Open } else { r.upperBoundType & i.upperBoundType }

      asRange(lb, lbt, ub, ubt)
    }

    def unary_-(): Interval[Int] = {
      Interval(-r.upperEndpoint, r.upperBoundType, -r.lowerEndpoint, r.lowerBoundType)
    }

    def -(i: Interval[Int]) = this + -i

    def -(v: Int) = this + -v

    def *(i: Interval[Int]): Interval[Int] = {

      val (a, bc) = asInfinities(r.canonical(IntDiscreteDomain))
      val (c, dc) = asInfinities(i.canonical(IntDiscreteDomain))

      val d = dc - Finite(1)
      val b = bc - Finite(1)

      val l = List(a * c, a * d, b * c, b * d).min

      val u = List(a * c, a * d, b * c, b * d).max

      asRange(l, Closed, u, Closed)

    }

    def /(i: Interval[Int]) = {
      if (i.contains(0)) {
        Interval.all[Int]
      } else {
        val (a, bc) = asInfinities(r.canonical(IntDiscreteDomain))
        val (c, dc) = asInfinities(i.canonical(IntDiscreteDomain))

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

    def /(v: Int) = {
      if (v == 0) {
        Interval.all[Int]
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

    def abs: Interval[Int] = {
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
