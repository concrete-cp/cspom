package cspom.variable

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Gen

object IntervalTest {
  def validInterval(i: Int, j: Int) = i <= j && j.toLong - i <= Int.MaxValue

  def validIntervals =
    for (i <- Gen.choose(Int.MinValue, Int.MaxValue); j: Long <- Gen.choose(i, math.min(Int.MaxValue, i.toLong + Int.MaxValue)))
      yield new Interval(i, j.toInt)

  def smallIntervals =
    for (i <- Gen.choose(Int.MinValue, Int.MaxValue); j: Long <- Gen.choose(i, math.min(Int.MaxValue, i.toLong + 1000)))
      yield new Interval(i, j.toInt)
}

class IntervalTest extends FlatSpec with Matchers with PropertyChecks {

  "Intervals" should "provide correct shrinking integer division" in {
    Interval(1, 19) / 20 shouldBe empty
    Interval(0, 19) / 20 shouldBe Interval(0, 0)
    Interval(1, 20) / 20 shouldBe Interval(1, 1)
    Interval(0, 20) / 20 shouldBe Interval(0, 1)

    Interval(1, 19) / -20 shouldBe empty
    Interval(0, 19) / -20 shouldBe Interval(0, 0)
    Interval(1, 20) / -20 shouldBe Interval(-1, -1)
    Interval(0, 20) / -20 shouldBe Interval(-1, 0)
  }

  it should "accept empty intervals" in {
    forAll { (i: Int, j: Int) =>
      whenever(i > j) {
        val itv = Interval(i, j)

        itv shouldBe empty
        itv.range shouldBe empty
        itv.size shouldBe 0
        forAll { (k: Int) => itv.contains(k) shouldBe false }
      }
    }
  }

  it should "complain about large intervals" in {
    an[Exception] should be thrownBy Interval(-1500000000, 1500000000)
  }

  it should "compare correctly" in {
    Interval(0, 5) isBefore Interval(7, 10) shouldBe true
    Interval(0, 5) isBefore Interval(6, 10) shouldBe false
    Interval(7, 10) isAfter Interval(0, 5) shouldBe true
    Interval(6, 10) isAfter Interval(0, 5) shouldBe false
    Interval(0, 5) intersects Interval(7, 10) shouldBe false
    Interval(0, 5) intersects Interval(6, 10) shouldBe false
    Interval(0, 5) intersects Interval(5, 10) shouldBe true
  }

  it should "detect mergeability" in {
    Interval(0, 5) isMergeableWith Interval(6, 10) shouldBe true
    Interval(0, 5) isMergeableWith Interval(-6, -1) shouldBe true
    Interval(0, 5) isMergeableWith Interval(-6, 2) shouldBe true
    Interval(0, 5) isMergeableWith Interval(-6, 20) shouldBe true
    Interval(0, 5) isMergeableWith Interval(10, 20) shouldBe false
    Interval(0, 5) isMergeableWith Interval(-20, -10) shouldBe false
  }

}