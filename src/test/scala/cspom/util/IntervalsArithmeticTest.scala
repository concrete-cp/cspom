package cspom.util

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import IntervalsArithmetic.Arithmetics
import IntervalsArithmetic.RangeArithmetics
import RangeSet.rangeAsRangeSet
import RangeSet.valueAsSingletonRange
import RangeSet.valueasRangeSet

class IntervalsArithmeticTest extends FlatSpec with Matchers {
  "Intervals" should "provide correct shrinking integer division" in {
    Interval(1, 19) / 20 shouldBe empty
    Interval(0, 19) / 20 shouldBe Interval(0, 0)
    Interval(1, 20) / 20 shouldBe Interval(1, 1)
    Interval(0, 20) / 20 shouldBe Interval(0, 1)

    Interval(1, 19) / -20 shouldBe empty
    Interval(0, 19) / -20 shouldBe Interval(0, 0)
    Interval(1, 20) / -20 shouldBe Interval(-1, -1)
    Interval(0, 20) / -20 shouldBe Interval(-1, 0)

    Interval.atLeast(2) / 20 shouldBe Interval.atLeast(1)
  }

  it should "multiply" in {
    Interval(0, 5) * 10 shouldBe Interval(0, 50)
    (RangeSet(Interval(0, 100)) -- Interval(10, 90)) * 10 shouldBe
      RangeSet(Seq(Interval(0, 90), Interval(910, 1000)))

    Interval.atLeast(10) * 100 shouldBe Interval.atLeast(1000)
    Interval.greaterThan(9) * 100 shouldBe Interval.atLeast(1000)
  }

  it should "compute abs" in {
    Interval(5, 10).abs shouldBe Interval(5, 10)
    Interval(-10, -5).abs shouldBe Interval(5, 10)
    Interval(-10, 0).abs shouldBe Interval(0, 10)
    Interval(0, 10).abs shouldBe Interval(0, 10)
    Interval(-5, 10).abs shouldBe Interval(0, 10)
  }

  it should "add" in {

    ((RangeSet(Interval(0, 100)) -- Interval(10, 90)) + Interval(0, 10)).canonical(IntDiscreteDomain) shouldBe
      RangeSet(Seq(Interval(0, 19), Interval(91, 110))).canonical(IntDiscreteDomain)

  }

  it should "handle infinities" in {
    Interval(1, 1) + Interval.atLeast(0) shouldBe Interval.atLeast(1)
    Interval(1, 1) * Interval.atLeast(0) shouldBe Interval.atLeast(0)
    Interval(1, 1) / Interval.atLeast(1) shouldBe Interval(0, 1)
    Interval.atLeast(10) / 2 shouldBe Interval.atLeast(5)
    Interval.all[Int] / 2 shouldBe Interval.all[Int]
  }

}