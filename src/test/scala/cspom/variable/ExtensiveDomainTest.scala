package cspom.variable;

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.prop.PropertyChecks

import cspom.util.ContiguousIntRangeSet
import cspom.util.IntInterval
import cspom.util.RangeSet

final class ExtensiveDomainTest extends FlatSpec with Matchers with PropertyChecks {

  "Extensive domains" should "behave like sets" in {
    val d = List(0, 0, -2147483648)

    val intDomain = new ContiguousIntRangeSet(
      RangeSet(d.map(IntInterval.singleton(_))))

    intDomain should contain(0)
    intDomain should contain(-2147483648)
    intDomain should have size 2

    d.toSet shouldBe intDomain

    forAll { d: Seq[Int] =>

      whenever(d.nonEmpty) {

        val intDomain = new ContiguousIntRangeSet(
          RangeSet(d.map(IntInterval.singleton(_))))

        d.toSet shouldBe intDomain

      }

    }
  }

}
