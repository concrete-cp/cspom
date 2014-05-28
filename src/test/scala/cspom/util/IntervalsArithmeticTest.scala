package cspom.util

import org.scalatest.Matchers
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalatest.FlatSpec

import Intervals.Interval

import IntervalsArithmetic._

import RangeSet._

object Intervals {
  def validInterval(i: Int, j: Int) = i <= j

  def Interval(l: Int, b: Int) = GuavaRange.closed(l, b)

  def validIntervals =
    for (
      i <- Arbitrary.arbitrary[Int];
      j: Int <- Gen.choose(i, Int.MaxValue)
    ) yield Interval(i, j)

  def smallIntervals =
    for (
      i <- Arbitrary.arbitrary[Int];
      j: Int <- Gen.choose(i, math.min(Int.MaxValue, i.toLong + 1000).toInt)
    ) yield Interval(i, j)
}

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

    GuavaRange.atLeast(2) / 20 shouldBe GuavaRange.atLeast(1)
  }

  it should "multiply" in {
    Interval(0, 5) * 10 shouldBe Interval(0, 50)
    (RangeSet(Interval(0, 100)) - Interval(10, 90)) * 10 shouldBe
      RangeSet(Seq(Interval(0, 90), Interval(910, 1000)))

    GuavaRange.atLeast(10) * 100 shouldBe GuavaRange.atLeast(1000)
    GuavaRange.greaterThan(9) * 100 shouldBe GuavaRange.atLeast(1000)
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

}