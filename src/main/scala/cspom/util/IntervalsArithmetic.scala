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
    for (i <- ii.ranges; j <- jj.ranges) {
      result ++= f(i, j)
    }
    result
  }

  def apply(f: IntInterval => IntInterval, ii: IntRangeSet): IntRangeSet = {
    ii.ranges.foldLeft(IntRangeSet())(_ ++ f(_))
  }

  private def asRange(l: Infinitable, u: Infinitable) = {
    if (l == MinInf && u == PlusInf) {
      IntInterval.all
    } else {
      IntInterval(l, u)
    }
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
      val IntInterval(a, b) = r
      val IntInterval(c, d) = i

      val lb = a + c

      val ub = b + d

      asRange(lb, ub)
    }

    def unary_-(): IntInterval = {
      val IntInterval(a, b) = r

      asRange(-b, -a)
    }

    def -(i: IntInterval) = this + -i

    def -(v: Int) = this + -v

    def *(i: IntInterval): IntInterval = {

      val IntInterval(a, b) = r
      val IntInterval(c, d) = i

      val l = List(a * c, a * d, b * c, b * d).min

      val u = List(a * c, a * d, b * c, b * d).max

      asRange(l, u)

    }

    def /(i: IntInterval): IntInterval = {
      if (i.contains(0)) {
        IntInterval.all
      } else {
        val IntInterval(a, b) = r
        val IntInterval(c, d) = i

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

    def /(v: Int): IntInterval = {
      if (v == 0) {
        IntInterval.all
      } else {
        val IntInterval(lb, ub) = r
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

    def abs: IntInterval = {
      val IntInterval(l, u) = r
      if (u <= Finite(0)) { -r }
      else if (l >= Finite(0)) { r }
      else {
        val b = if (u > -l) u else -l

        asRange(Finite(0), b)
      }
    }

  }
}
