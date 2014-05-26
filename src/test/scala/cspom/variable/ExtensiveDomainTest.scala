package cspom.variable;

import scala.collection.SortedSet
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.prop.PropertyChecks
import cspom.util.RangeSet
import cspom.util.GuavaRange
import com.google.common.collect.ContiguousSet
import cspom.util.IntDiscreteDomain

final class ExtensiveDomainTest extends FlatSpec with Matchers with PropertyChecks {

  "Extensive domains" should "behave like sets" in {
    forAll { d: Seq[Int] =>

      whenever(d.nonEmpty) {

        val intDomain = RangeSet(d.map(GuavaRange.singleton(_))).toSet(IntDiscreteDomain)

        d should contain theSameElementsAs intDomain

      }

    }
  }

}
