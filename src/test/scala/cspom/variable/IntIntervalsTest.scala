package cspom.variable;

import scala.collection.SortedSet

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.concurrent.Timeouts
import org.scalatest.prop.PropertyChecks
import org.scalatest.time.Second
import org.scalatest.time.Span

final class IntIntervalsTest extends FlatSpec with Matchers with PropertyChecks with Timeouts {

  import IntervalTest._

  "IntInterval" should "not accept empty domains" in
    forAll { (i: Int, j: Int) =>
      whenever(!validInterval(i, j)) {
        an[IllegalArgumentException] should be thrownBy IntDomain(Interval(i, j))
      }
    }

  it should "check interval equality" in
    forAll(validIntervals, validIntervals) { (itv1, itv2) =>

      if (itv1.lb == itv2.lb && itv1.ub == itv2.ub) {
        itv1 shouldBe itv2
        itv2 shouldBe itv1
      } else {
        itv1 should not be (itv2)
        itv2 should not be (itv1)
      }
    }

  it should "check equality on any set" in forAll(validIntervals, Arbitrary.arbitrary[Seq[Int]]) {
    (itv, seq) =>
      whenever(seq.nonEmpty) {
        val s = seq.toSet
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

  }

  it should "generate proper set of values" in forAll(smallIntervals) { interval =>
    failAfter(Span(1, Second)) {
      val Interval(i, j) = interval

      val intInterval = IntDomain(interval)

      if (i == j) {
        intInterval.toString shouldBe s"[$i]"
      } else {
        intInterval.toString shouldBe s"[$i..$j]"
      }
      intInterval shouldBe an[IntIntervals]

      intInterval.asInstanceOf[IntIntervals].intervals should contain theSameElementsAs (i to j)
    }

  }

}
