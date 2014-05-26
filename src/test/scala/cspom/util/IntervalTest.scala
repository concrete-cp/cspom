package cspom.util

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import Intervals.Interval

class IntervalTest extends FlatSpec with Matchers  {

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