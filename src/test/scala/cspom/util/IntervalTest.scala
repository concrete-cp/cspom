package cspom.util

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

class IntervalTest extends FlatSpec with Matchers {

  "Intervals" should "compare correctly" in {
    assert(IntInterval(0, 5) isBefore IntInterval(7, 10))
    assert(IntInterval(0, 5) isConnected IntInterval(6, 10))
    assert(IntInterval(7, 10) isAfter IntInterval(0, 5))
    assert(IntInterval(6, 10) isConnected IntInterval(0, 5))
    IntInterval(0, 5) & IntInterval(6, 10) shouldBe empty
    IntInterval(0, 5) & IntInterval(5, 10) should not be empty
  }

  it should "detect mergeability" in {
    assert(IntInterval(0, 5) isConnected IntInterval(6, 10))
    assert(IntInterval(0, 5) isConnected IntInterval(-6, -1))
    assert(IntInterval(0, 5) isConnected IntInterval(-6, 2))
    assert(IntInterval(0, 5) isConnected IntInterval(-6, 20))
    assert(!(IntInterval(0, 5) isConnected IntInterval(10, 20)))
    assert(!(IntInterval(0, 5) isConnected IntInterval(-20, -10)))
    assert(IntInterval(Int.MinValue, 0) isConnected IntInterval(0, 5))
  }

  it should "detect equality" in {
    IntInterval.all should not be IntInterval.atLeast(0)
  }

  it should "be iterable" in {
    val i = IntInterval(0, 0)
    val l = i.toList

    l should contain theSameElementsInOrderAs Iterable(0)
  }

}

object Intervals {
  def validInterval(i: Int, j: Int) = i <= j

  def validIntervals =
    for (
      i <- Arbitrary.arbitrary[Int];
      j: Int <- Gen.choose(i, Int.MaxValue)
    ) yield IntInterval(i, j)

  def smallIntervals =
    for (
      i <- Arbitrary.arbitrary[Int];
      j: Int <- Gen.choose(i, math.min(Int.MaxValue, i.toLong + 1000).toInt)
    ) yield IntInterval(i, j)
}
