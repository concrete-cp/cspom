package cspom.util

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.prop.PropertyChecks

import IntervalsArithmetic._

import IntervalTest.Interval

object IntervalTest {
  def validInterval(i: Int, j: Int) = i <= j && j.toLong - i <= Int.MaxValue

  def Interval(l: Int, b: Int) = GuavaRange.ofIntInterval(l, b)

  def validIntervals =
    for (
      i <- Arbitrary.arbitrary[Int].suchThat(_ < Int.MaxValue);
      j: Int <- Gen.choose(i, math.min(Int.MaxValue, i.toLong + Int.MaxValue).toInt)
    ) yield Interval(i, j.toInt)

  def smallIntervals =
    for (i <- Arbitrary.arbitrary[Int]; j: Int <- Gen.choose(i, math.min(Int.MaxValue, i.toLong + 1000).toInt))
      yield Interval(i, j.toInt)
}

class IntervalTest extends FlatSpec with Matchers with PropertyChecks {

  "Intervals" should "provide correct shrinking integer division" in {
    Interval(1, 19) / 20 shouldBe empty
    canonical(Interval(0, 19) / 20) shouldBe Interval(0, 0)
    canonical(Interval(1, 20) / 20) shouldBe Interval(1, 1)
    canonical(Interval(0, 20) / 20) shouldBe Interval(0, 1)

    Interval(1, 19) / -20 shouldBe empty
    canonical(Interval(0, 19) / -20) shouldBe Interval(0, 0)
    canonical(Interval(1, 20) / -20) shouldBe Interval(-1, -1)
    canonical(Interval(0, 20) / -20) shouldBe Interval(-1, 0)

    GuavaRange.atLeast(2) / 20 shouldBe GuavaRange.greaterThan(0)
  }

  it should "compare correctly" in {
    Interval(0, 5) isBefore Interval(7, 10) shouldBe true
    Interval(0, 5) isBefore Interval(6, 10) shouldBe false
    Interval(7, 10) isAfter Interval(0, 5) shouldBe true
    Interval(6, 10) isAfter Interval(0, 5) shouldBe false
    Interval(0, 5) & Interval(6, 10) shouldBe empty
    Interval(0, 5) & Interval(5, 10) should not be empty
  }

  it should "detect mergeability" in {
    Interval(0, 5) isConnected Interval(6, 10) shouldBe true
    Interval(0, 5) isConnected Interval(-6, -1) shouldBe true
    Interval(0, 5) isConnected Interval(-6, 2) shouldBe true
    Interval(0, 5) isConnected Interval(-6, 20) shouldBe true
    Interval(0, 5) isConnected Interval(10, 20) shouldBe false
    Interval(0, 5) isConnected Interval(-20, -10) shouldBe false
  }

}