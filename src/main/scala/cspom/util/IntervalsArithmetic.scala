package cspom.util

import java.math.RoundingMode
import java.math.RoundingMode
import com.google.common.math.IntMath

object IntervalsArithmetic {

  def apply[A <% Ordered[A]](
    f: (GuavaRange[A], GuavaRange[A]) => GuavaRange[A],
    ii: RangeSet[A], jj: RangeSet[A]): RangeSet[A] = {
    var result = RangeSet.empty[A]
    for (i <- ii.ranges; j <- jj.ranges) {
      // + means union here
      result += f(i, j)
    }
    result
  }

  def apply[A <% Ordered[A]](f: GuavaRange[A] => GuavaRange[A], ii: RangeSet[A]): RangeSet[A] = {
    ii.ranges.map(f).foldLeft(RangeSet.empty[A])(_ + _)
  }

  def canonical(r: GuavaRange[Int]) = {
    val lower: GuavaRange[Int] =
      if (r.hasLowerBound) {
        val l = if (r.lowerBoundType == Open) {
          IntMath.checkedAdd(r.lowerEndpoint, 1)
        } else {
          r.lowerEndpoint
        }
        GuavaRange.downTo(l, Closed)
      } else {
        GuavaRange.all[Int]
      }

    val upper: GuavaRange[Int] =
      if (r.hasUpperBound) {
        val u = if (r.upperBoundType == Closed) {
          IntMath.checkedAdd(r.upperEndpoint, 1)
        } else {
          r.upperEndpoint
        }
        GuavaRange.upTo(u, Open)
      } else {
        GuavaRange.all[Int]
      }

    lower & upper

  }

  private def asInfinities(r: GuavaRange[Int]) = {
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

  private def asRange(l: Value, lbt: BoundType, u: Value, ubt: BoundType) = {
    (l, u) match {
      case (Finite(l), Finite(u)) => GuavaRange(l, lbt, u, ubt)
      case (MinInf, Finite(u)) => GuavaRange.upTo(u, ubt)
      case (Finite(l), PlusInf) => GuavaRange.downTo(l, lbt)
      case (MinInf, PlusInf) => GuavaRange.all[Int]
      case _ => throw new IllegalStateException
    }
  }

  private def minBound[A](b: (A, BoundType)*)(implicit ord: Ordering[A]): (A, BoundType) = {
    b.min(Ordering.Tuple2(ord, BoundType.closedIsLess))
  }

  private def maxBound[A](b: (A, BoundType)*)(implicit ord: Ordering[A]): (A, BoundType) = {
    b.max(Ordering.Tuple2(ord, BoundType.closedIsMore))
  }

  implicit class Arithmetics(r: GuavaRange[Int]) {
    /**
     * [a, b] + [c, d] = [a + c, b + d]
     * [a, b] − [c, d] = [a − d, b − c]
     * [a, b] × [c, d] = [min (a × c, a × d, b × c, b × d), max (a × c, a × d, b × c, b × d)]
     * [a, b] ÷ [c, d] = [min (a ÷ c, a ÷ d, b ÷ c, b ÷ d), max (a ÷ c, a ÷ d, b ÷ c, b ÷ d)] when 0 is not in [c, d].
     */

    def +(i: GuavaRange[Int]): GuavaRange[Int] = {
      val (a, b) = asInfinities(r)
      val (c, d) = asInfinities(i)

      asRange(a + c, r.lowerBoundType & i.lowerBoundType, b + d, r.upperBoundType & i.upperBoundType)
    }

    def +(v: Int): GuavaRange[Int] = this + GuavaRange.ofInt(v)

    def unary_-(): GuavaRange[Int] = {
      GuavaRange(-r.upperEndpoint, r.upperBoundType, -r.lowerEndpoint, r.lowerBoundType)
    }

    def -(i: GuavaRange[Int]) = this + -i

    def -(v: Int) = this + -v

    def *(i: GuavaRange[Int]): GuavaRange[Int] = {
      val (a, b) = asInfinities(r)
      val (c, d) = asInfinities(i)

      val (l, lbt) = minBound(
        (a * c, r.lowerBoundType & i.lowerBoundType),
        (a * d, r.lowerBoundType & i.upperBoundType),
        (b * c, r.upperBoundType & i.lowerBoundType),
        (b * d, r.upperBoundType & i.upperBoundType))

      val (u, ubt) = maxBound(
        (a * c, r.lowerBoundType & i.lowerBoundType),
        (a * d, r.lowerBoundType & i.upperBoundType),
        (b * c, r.upperBoundType & i.lowerBoundType),
        (b * d, r.upperBoundType & i.upperBoundType))

      asRange(l, lbt, u, ubt)

    }

    def *(v: Int): GuavaRange[Int] = this * GuavaRange.ofInt(v)

    def /(i: GuavaRange[Int]) = {
      if (i.contains(0)) {
        GuavaRange.all[Int]
      } else {
        val (a, b) = asInfinities(r)
        val (c, d) = asInfinities(i)

        val (l, lbt) = minBound(
          (a.div(c, RoundingMode.CEILING), r.lowerBoundType & i.lowerBoundType),
          (a.div(d, RoundingMode.CEILING), r.lowerBoundType & i.upperBoundType),
          (b.div(c, RoundingMode.CEILING), r.upperBoundType & i.lowerBoundType),
          (b.div(d, RoundingMode.CEILING), r.upperBoundType & i.upperBoundType))

        val (u, ubt) = maxBound(
          (a.div(c, RoundingMode.FLOOR), r.lowerBoundType & i.lowerBoundType),
          (a.div(d, RoundingMode.FLOOR), r.lowerBoundType & i.upperBoundType),
          (b.div(c, RoundingMode.FLOOR), r.upperBoundType & i.lowerBoundType),
          (b.div(d, RoundingMode.FLOOR), r.upperBoundType & i.upperBoundType))

        asRange(l, lbt, u, ubt)
      }
    }

    def /(v: Int) = {
      val (lb, ub) = asInfinities(r)
      val d = Finite(v)
      if (v >= 0) {
        asRange(
          lb.div(d, RoundingMode.CEILING), if (lb.divisible(d)) r.lowerBoundType else Closed,
          ub.div(d, RoundingMode.FLOOR), if (ub.divisible(d)) r.upperBoundType else Closed)
      } else {
        asRange(
          ub.div(d, RoundingMode.CEILING), if (ub.divisible(d)) r.upperBoundType else Closed,
          lb.div(d, RoundingMode.FLOOR), if (lb.divisible(d)) r.lowerBoundType else Closed)
      }
    }

    def abs: GuavaRange[Int] = {
      val (l, u) = asInfinities(r)
      if (u <= Finite(0)) { -r }
      else if (l >= Finite(0)) { r }
      else {
        val (b, bt) = maxBound[Value]((-l, r.lowerBoundType), (u, r.upperBoundType))

        asRange(Finite(0), Closed, b, bt)
      }
    }

  }
}
