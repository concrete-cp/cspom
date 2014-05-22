package cspom.util

object IntervalsArithmetic {

  //  def apply(f: (Interval, Interval) => Interval, ii: Intervals, jj: Intervals): Intervals = {
  //    var result = Intervals.empty
  //    for (i <- ii.intervals; j <- jj.intervals) {
  //      // + means union here
  //      result += f(i, j)
  //    }
  //    result
  //  }
  //
  //  def apply(f: Interval => Interval, ii: Intervals): Intervals = {
  //    ii.intervals.map(f).foldLeft(Intervals.empty)(_ + _)
  //  }

  def asInfinities(r: GuavaRange[Int]) = {
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

  def minBound[A](b: (A, BoundType)*)(implicit ord: Ordering[A]): (A, BoundType) = {
    b.min(Ordering.Tuple2(ord, BoundType.closedIsLess))
  }

  def maxBound[A](b: (A, BoundType)*)(implicit ord: Ordering[A]): (A, BoundType) = {
    b.max(Ordering.Tuple2(ord, BoundType.closedIsMore))
  }

  implicit class IntervalsArithmetic(r: GuavaRange[Int]) {
    /**
     * [a, b] + [c, d] = [a + c, b + d]
     * [a, b] − [c, d] = [a − d, b − c]
     * [a, b] × [c, d] = [min (a × c, a × d, b × c, b × d), max (a × c, a × d, b × c, b × d)]
     * [a, b] ÷ [c, d] = [min (a ÷ c, a ÷ d, b ÷ c, b ÷ d), max (a ÷ c, a ÷ d, b ÷ c, b ÷ d)] when 0 is not in [c, d].
     */

    def +(i: GuavaRange[Int]): GuavaRange[Int] = {
      GuavaRange(r.lowerEndpoint + i.lowerEndpoint, r.lowerBoundType & i.lowerBoundType,
        r.upperEndpoint + i.upperEndpoint, r.upperBoundType & i.upperBoundType)
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

      (l, u) match {
        case (Finite(l), Finite(u)) => GuavaRange(l, lbt, u, ubt)
        case (MinInf, Finite(u)) => GuavaRange.upTo(u, ubt)
        case (Finite(l), PlusInf) => GuavaRange.downTo(l, lbt)
        case (MinInf, PlusInf) => GuavaRange.all[Int]
        case _ => throw new IllegalStateException
      }

    }

    def *(v: Int): GuavaRange[Int] = this * GuavaRange.ofInt(v)
    //
    //    def /(i: Interval) = {
    //      if (i.contains(0)) throw new ArithmeticException
    //      val Interval(c, d) = i
    //      Interval(
    //        List(ceilDiv(lb, c), ceilDiv(lb, d), ceilDiv(ub, c), ceilDiv(ub, d)).min,
    //        List(floorDiv(lb, c), floorDiv(lb, d), floorDiv(ub, c), floorDiv(ub, d)).max)
    //    }
    //
    //    def /(v: Int) = {
    //      if (v >= 0) {
    //        Interval(ceilDiv(lb, v), floorDiv(ub, v))
    //      } else {
    //        Interval(ceilDiv(ub, v), floorDiv(lb, v))
    //      }
    //
    //      //    if (l < u) Interval(l, u) else Interval(u, l)
    //    }
    //
    //    def floorDiv(dividend: Int, divisor: Int) = {
    //      val roundedTowardsZeroQuotient = dividend / divisor;
    //      val dividedEvenly = (dividend % divisor) == 0;
    //      if (dividedEvenly) {
    //        roundedTowardsZeroQuotient;
    //      } else {
    //        // If they're of opposite sign then we rounded 
    //        // UP towards zero so we rem one. If they're of the same sign then 
    //        // we rounded DOWN towards zero, so we are done.
    //
    //        if (divisor.signum == dividend.signum) {
    //          roundedTowardsZeroQuotient;
    //        } else {
    //          roundedTowardsZeroQuotient - 1;
    //        }
    //      }
    //    }
    //
    //    def ceilDiv(dividend: Int, divisor: Int) = {
    //
    //      val roundedTowardsZeroQuotient = dividend / divisor;
    //      val dividedEvenly = (dividend % divisor) == 0;
    //      if (dividedEvenly) {
    //        roundedTowardsZeroQuotient;
    //      } else {
    //        // If they're of opposite sign then we rounded 
    //        // UP towards zero so we're done. If they're of the same sign then 
    //        // we rounded DOWN towards zero, so we need to add one.
    //
    //        if (divisor.signum == dividend.signum) {
    //          roundedTowardsZeroQuotient + 1;
    //        } else {
    //          roundedTowardsZeroQuotient;
    //        }
    //      }
    //    }
    //
    //  }
  }
}
