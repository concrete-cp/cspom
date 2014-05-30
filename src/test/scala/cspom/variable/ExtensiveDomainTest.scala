package cspom.variable;

import scala.collection.SortedSet

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.prop.PropertyChecks

import cspom.util.ContiguousRangeSet
import cspom.util.Interval
import cspom.util.IntDiscreteDomain
import cspom.util.RangeSet

final class ExtensiveDomainTest extends FlatSpec with Matchers with PropertyChecks {

  implicit def asSet(r: RangeSet[Int]): SortedSet[Int] =
    new ContiguousRangeSet(r, IntDiscreteDomain)

  "Extensive domains" should "behave like sets" in {
    forAll { d: Seq[Int] =>

      whenever(d.nonEmpty) {

        val intDomain = asSet(RangeSet(d.map(Interval.singleton(_))))

        d.toSet shouldBe intDomain

      }

    }
  }

}
