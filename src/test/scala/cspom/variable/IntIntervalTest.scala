package cspom.variable;

import org.junit.Test
import org.junit.Before
import org.junit.Assert._
import cspom.xcsp.XCSPParser
import org.scalatest.prop.PropertyChecks
import org.scalatest.Matchers
import org.scalatest.FlatSpec
import scala.collection.SortedSet
import org.scalatest.concurrent.Timeouts
import org.scalatest.time.Span
import org.scalatest.time.Second
import org.scalacheck.Gen
import org.scalacheck.Arbitrary

final class IntIntervalTest extends FlatSpec with Matchers with PropertyChecks with Timeouts {

  def validInterval(i: Int, j: Int) = i <= j && j.toLong - i <= Int.MaxValue

  def validIntervals =
    for (i <- Gen.choose(Int.MinValue, Int.MaxValue); j: Long <- Gen.choose(i.toLong, i + Int.MaxValue) if j <= Int.MaxValue) yield new IntInterval(i, j.toInt)

  "IntInterval" should "not accept empty domains" in
    forAll { (i: Int, j: Int) =>
      whenever(!validInterval(i, j)) {
        an[IllegalArgumentException] should be thrownBy new IntInterval(i, j)
      }
    }

  it should "check interval equality" in
    forAll(validIntervals, validIntervals) { (itv1, itv2) =>

      whenever(itv1.lb == itv2.lb && itv1.ub == itv2.ub) {
        itv1 shouldBe itv2
        itv2 shouldBe itv1
      }

      whenever(!(itv1.lb == itv2.lb && itv1.ub == itv2.ub)) {
        itv1 should not be (itv2)
        itv2 should not be (itv1)
      }
    }

  it should "check equality on any set" in forAll(validIntervals, Arbitrary.arbitrary[SortedSet[Int]]) {
    (itv, s: SortedSet[Int]) =>

      failAfter(Span(1, Second)) {
        if (s.min == itv.lb && s.max == itv.ub && s.size == itv.size) {
          itv shouldBe s
          s shouldBe itv
        } else {
          itv should not be (s)
          s should not be (itv)
        }
      }

  }

  it should "generate proper set of values" in forAll(validIntervals) { intInterval =>
    failAfter(Span(1, Second)) {
      val IntInterval(i, j) = intInterval

      println("Set " + intInterval)

      intInterval.toString shouldBe s"[$i..$j]"
      intInterval shouldBe (i to j)
      (i to j) shouldBe intInterval
    }
  }

}
