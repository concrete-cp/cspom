package cspom.variable

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.concurrent.Timeouts
import org.scalatest.prop.PropertyChecks
import org.scalatest.time.Second
import org.scalatest.time.Span

import cspom.xcsp.XCSPParser
class IntDomainTest extends FlatSpec with Matchers with PropertyChecks with Timeouts {

  "XCSP domain parser" should "handle disjoint values and intervals" in {
    XCSPParser.parseDomain("1 3..10 18..20") shouldBe Set(1) ++ (3 to 10) ++ (18 to 20)
  }

  it should "handle single intervals" in forAll(IntervalTest.validIntervals) {
    case itv @ Interval(i, j) =>

      val parsed = XCSPParser.parseItv(s"$i..$j")

      parsed shouldBe a[Interval]

      failAfter(Span(1, Second)) {
        parsed shouldBe itv
      }

  }

  it should "handle joint intervals" in {
    val itv = XCSPParser.parseDomain("3..5 6 7..10")
    itv shouldBe a[IntIntervals]
    itv shouldBe IntIntervals(Intervals(3, 10))
  }

}