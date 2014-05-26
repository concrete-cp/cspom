package cspom.variable

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.concurrent.Timeouts
import org.scalatest.prop.PropertyChecks
import org.scalatest.time.Second
import org.scalatest.time.Span
import cspom.xcsp.XCSPParser
import cspom.util.IntervalTest
import cspom.util.GuavaRange
import cspom.util.RangeSet
import cspom.util.Intervals
import cspom.util.IntDiscreteDomain
class IntDomainTest extends FlatSpec with Matchers with PropertyChecks with Timeouts {

  "XCSP domain parser" should "handle disjoint values and intervals" in {
    IntDiscreteDomain.allValues(
      XCSPParser.parseDomain("1 3..10 18..20")) should contain theSameElementsAs (1 +: (3 to 10) ++: (18 to 20))
  }

  it should "parse ranges" in forAll(Intervals.validIntervals) {
    r =>

      val i = r.lowerEndpoint
      val j = r.upperEndpoint

      val parsed = XCSPParser.parseRange(s"$i..$j")

      parsed shouldBe a[GuavaRange[_]]

      failAfter(Span(1, Second)) {
        parsed shouldBe r
      }

  }

  it should "coalesce connected ranges" in {
    val itv = XCSPParser.parseDomain("3..5 6 7..10")
    itv shouldBe a[RangeSet[_]]
    itv shouldBe RangeSet(GuavaRange.closed(3, 10))
  }

}