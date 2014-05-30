package cspom.variable

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.concurrent.Timeouts
import org.scalatest.prop.PropertyChecks
import org.scalatest.time.Second
import org.scalatest.time.Span
import cspom.xcsp.XCSPParser
import cspom.util.IntervalTest
import cspom.util.Interval
import cspom.util.RangeSet
import cspom.util.Intervals
import cspom.util.IntDiscreteDomain
import cspom.util.ContiguousRangeSet
import scala.collection.SortedSet
class IntDomainTest extends FlatSpec with Matchers with PropertyChecks with Timeouts {

  implicit def asSet(r: RangeSet[Int]): SortedSet[Int] =
    new ContiguousRangeSet(r, IntDiscreteDomain)

  "XCSP domain parser" should "handle disjoint values and intervals" in {
    XCSPParser.parseDomain("1 3..10 18..20").toSeq shouldBe
      1 +: (3 to 10) ++: (18 to 20)
  }

  it should "parse ranges" in forAll(Intervals.validIntervals) {
    r =>

      val i = r.lowerEndpoint
      val j = r.upperEndpoint

      val parsed = XCSPParser.parseRange(s"$i..$j")

      parsed shouldBe a[Interval[_]]

      failAfter(Span(1, Second)) {
        parsed shouldBe r
      }

  }

  it should "coalesce connected ranges" in {
    val itv = XCSPParser.parseDomain("3..5 6 7..10").canonical(IntDiscreteDomain)
    itv shouldBe a[RangeSet[_]]
    itv shouldBe RangeSet(Interval(3, 10).canonical(IntDiscreteDomain))
  }

}