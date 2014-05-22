package cspom.variable;

import scala.collection.SortedSet
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.prop.PropertyChecks
import cspom.util.RangeSet
import cspom.util.GuavaRange

final class ExtensiveDomainTest extends FlatSpec with Matchers with PropertyChecks {

  "Extensive domains" should "behave like sets" in {
    forAll { d: Seq[Int] =>

      whenever(d.nonEmpty) {

        val s = d.toSet

        val intDomain = RangeSet(d.map(GuavaRange.singleton(_)): _*)

        s shouldBe intDomain
        intDomain shouldBe s

      }

    }
  }

}
