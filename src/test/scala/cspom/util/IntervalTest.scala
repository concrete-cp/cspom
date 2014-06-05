package cspom.util

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

class IntervalTest extends FlatSpec with Matchers {

  it should "compare correctly" in {
    Interval(0, 5) isBefore Interval(7, 10) shouldBe true
    Interval(0, 5) isBefore Interval(6, 10) shouldBe true
    Interval(7, 10) isAfter Interval(0, 5) shouldBe true
    Interval(6, 10) isAfter Interval(0, 5) shouldBe true
    an[IllegalArgumentException] should be thrownBy Interval(0, 5) & Interval(6, 10)
    Interval(0, 5) & Interval(5, 10) should not be empty
  }

  it should "detect mergeability" in {
    Interval(0, 5).canonical(IntDiscreteDomain) isConnected Interval(6, 10) shouldBe true
    Interval(0, 5) isConnected Interval(-6, -1).canonical(IntDiscreteDomain) shouldBe true
    Interval(0, 5) isConnected Interval(-6, 2) shouldBe true
    Interval(0, 5) isConnected Interval(-6, 20) shouldBe true
    Interval(0, 5) isConnected Interval(10, 20) shouldBe false
    Interval(0, 5) isConnected Interval(-20, -10) shouldBe false
  }

  it should "detect equality" in {
    Interval.all[Int] should not be Interval.greaterThan(0)
  }

}

object Intervals {
  def validInterval(i: Int, j: Int) = i <= j

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
