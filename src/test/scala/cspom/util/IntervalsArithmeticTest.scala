package cspom.util

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import IntervalsArithmetic.Arithmetics
import IntervalsArithmetic.RangeArithmetics
import RangeSet.rangeAsRangeSet
import RangeSet.valueAsSingletonRange
import RangeSet.valueasRangeSet
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalatest.prop.PropertyChecks
import org.scalatest.time.Second
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.time.Span
import org.scalatest.concurrent.Timeouts

class IntervalsArithmeticTest extends FlatSpec with Matchers with PropertyChecks with Timeouts {
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
    Interval.all[Int] * 1 shouldBe Interval.all[Int]
    Interval(1, 1) + Interval.atLeast(0) shouldBe Interval.atLeast(1)
    Interval(1, 1) * Interval.atLeast(0) shouldBe Interval.atLeast(0)
    Interval(1, 1) / Interval.atLeast(1) shouldBe Interval(0, 1)
    Interval.atLeast(10) / 2 shouldBe Interval.atLeast(5)
    Interval.all[Int] / 2 shouldBe Interval.all[Int]

    Interval.all[Int] + 0 shouldBe Interval.all[Int]

  }

  it should "be fast" in {
    val g = Gen.listOfN(100, Gen.choose(-1000000, 1000000))

    implicit def intDom = IntDiscreteDomain

    forAll(g, g) { (r1, r2) =>
      val rs1 = RangeSet(r1.map(i => Interval.singleton(i)))
      val rs2 = RangeSet(r2.map(i => Interval.singleton(i)))

      failAfter(Span(1, Second)) {
        println(rs1 + rs2)
      }
    }

  }

}