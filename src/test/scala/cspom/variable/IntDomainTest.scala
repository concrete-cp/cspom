package cspom.variable

import scala.collection.SortedSet

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.concurrent.Timeouts
import org.scalatest.prop.PropertyChecks
import org.scalatest.time.Second
import org.scalatest.time.Span

import cspom.util.ContiguousIntRangeSet
import cspom.util.IntInterval
import cspom.util.IntRangeSet
import cspom.util.Intervals
import cspom.xcsp.XCSPParser

class IntDomainTest extends FlatSpec with Matchers with PropertyChecks with Timeouts {

  implicit def asSet(r: IntRangeSet): SortedSet[Int] =
    new ContiguousIntRangeSet(r)

  "XCSP domain parser" should "handle disjoint values and intervals" in {
    XCSPParser.parseDomain("1 3..10 18..20").toSeq shouldBe
      1 +: (3 to 10) ++: (18 to 20)
  }

  it should "parse ranges" in forAll(Intervals.validIntervals) {
    r =>

      val i = r.lowerEndpoint
      val j = r.upperEndpoint

      val parsed = XCSPParser.parseRange(s"$i..$j")

      parsed shouldBe a[IntInterval]

      failAfter(Span(1, Second)) {
        parsed shouldBe r
      }

  }

  it should "coalesce connected ranges" in {
    val itv = XCSPParser.parseDomain("3..5 6 7..10").canonical
    itv shouldBe a[IntRangeSet]
    itv shouldBe IntRangeSet(IntInterval(3, 10).canonical)
  }

}