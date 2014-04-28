package cspom.variable

import org.junit.Assert._
import org.junit.Test
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.prop.PropertyChecks
import cspom.xcsp.XCSPParser
import org.scalatest.concurrent.Timeouts
import org.scalatest.time.Span
import org.scalatest.time.Second
class IntDomainTest extends FlatSpec with Matchers with PropertyChecks with Timeouts {

  "XCSP domain parser" should "handle disjoint values and intervals" in {
    XCSPParser.parseDomain("1 3..10 18..20") shouldBe Set(1) ++ (3 to 10) ++ (18 to 20)
  }

  it should "handle single intervals" in forAll { (i: Int, j: Int) =>
    whenever(i <= j) {

      val itv = XCSPParser.parseItv(s"$i..$j")

      itv shouldBe a[IntInterval]
      
      failAfter(Span(1, Second)) {
        itv shouldBe new IntInterval(i, j)
      }
    }

  }

  it should "handle joint intervals" in {
    val itv = XCSPParser.parseDomain("3..5 6 7..10")
    itv shouldBe a[IntInterval]
    itv shouldBe new IntInterval(3, 10)
  }

}