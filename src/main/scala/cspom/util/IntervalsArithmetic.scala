package cspom.util

import java.math.RoundingMode
import Infinitable.compare

object IntervalsArithmetic {

  def apply(
    f: (Interval[Infinitable], Interval[Infinitable]) => Interval[Infinitable],
    ii: RangeSet[Infinitable], jj: RangeSet[Infinitable]): RangeSet[Infinitable] = {
    var result = RangeSet[Infinitable]()
    for (i <- ii.ranges; j <- jj.ranges) {
      result ++= f(i, j)
    }
    result
  }

  def apply(f: Interval[Infinitable] => Interval[Infinitable], ii: RangeSet[Infinitable]): RangeSet[Infinitable] = {
    ii.ranges.foldLeft(RangeSet[Infinitable]())(_ ++ f(_))
  }

  private def asRange(l: Infinitable, u: Infinitable) = {
    if (l == MinInf && u == PlusInf) {
      IntInterval.all
    } else {
      IntInterval(l, u)
    }
  }

  implicit class RangeArithmetics(val r: RangeSet[Infinitable]) extends AnyVal {
    def +(i: RangeSet[Infinitable]): RangeSet[Infinitable] = IntervalsArithmetic(_ + _, r, i)
    def unary_-(): RangeSet[Infinitable] = IntervalsArithmetic(-_, r)
    def -(i: RangeSet[Infinitable]): RangeSet[Infinitable] = IntervalsArithmetic(_ - _, r, i)
    def *(i: RangeSet[Infinitable]): RangeSet[Infinitable] = IntervalsArithmetic(_ * _, r, i)
    def /(i: RangeSet[Infinitable]): RangeSet[Infinitable] = IntervalsArithmetic(_ / _, r, i)
    def abs: RangeSet[Infinitable] = IntervalsArithmetic(_.abs, r)

    def +(i: Interval[Infinitable]) = IntervalsArithmetic({ r => r + i }, r)
    def -(i: Interval[Infinitable]) = IntervalsArithmetic({ r => r - i }, r)
    def *(i: Interval[Infinitable]) = IntervalsArithmetic({ r => r * i }, r)
    def /(i: Interval[Infinitable]) = IntervalsArithmetic({ r => r / i }, r)

    def /(i: Int) = IntervalsArithmetic({ r => r / i }, r)
  }

  implicit class Arithmetics(val r: Interval[Infinitable]) extends AnyVal {
    /**
     * [a, b] + [c, d] = [a + c, b + d]
     * [a, b] − [c, d] = [a − d, b − c]
     * [a, b] × [c, d] = [min (a × c, a × d, b × c, b × d), max (a × c, a × d, b × c, b × d)]
     * [a, b] ÷ [c, d] = [min (a ÷ c, a ÷ d, b ÷ c, b ÷ d), max (a ÷ c, a ÷ d, b ÷ c, b ÷ d)] when 0 is not in [c, d].
     */

    def +(i: Interval[Infinitable]): Interval[Infinitable] = {
      asRange(r.lb + i.lb, r.ub + i.ub)
    }

    def unary_-(): Interval[Infinitable] = {
      asRange(-r.ub, -r.lb)
    }

    def -(i: Interval[Infinitable]) = {
      asRange(r.lb - i.ub, r.ub - i.lb)
    }

    def *(i: Interval[Infinitable]): Interval[Infinitable] = {

      val Interval(a, b) = r
      val Interval(c, d) = i

      val l = Seq(a * c, a * d, b * c, b * d)

      asRange(l.min, l.max)

    }

    def /(i: Interval[Infinitable]): Interval[Infinitable] = {
      if (i.contains(Finite(0))) {
        IntInterval.all
      } else {
        val Interval(a, b) = r
        val Interval(c, d) = i

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

        asRange(l, u)
      }
    }

    def /(v: Int): Interval[Infinitable] = {
      if (v == 0) {
        IntInterval.all
      } else {
        val Interval(lb, ub) = r
        val d = Finite(v)
        if (v > 0) {
          asRange(
            lb.div(d, RoundingMode.CEILING),
            ub.div(d, RoundingMode.FLOOR))
        } else {
          asRange(
            ub.div(d, RoundingMode.CEILING),
            lb.div(d, RoundingMode.FLOOR))
        }
      }
    }

    def abs: Interval[Infinitable] = {
      val Interval(l, u) = r
      if (compare(u, 0) <= 0) { -r }
      else if (compare(l, 0) >= 0) { r }
      else {
        val b = if (Infinitable.InfinitableOrdering.compare(u, -l) > 0) u else -l

        asRange(Finite(0), b)
      }
    }

  }
}
