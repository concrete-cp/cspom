package cspom.variable;

import scala.collection.SortedSet

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.prop.PropertyChecks

final class ExtensiveDomainTest extends FlatSpec with Matchers with PropertyChecks {
  //
  //  var intDomain: IntDomain = null;
  //
  //  //var anyDomain: ExtensiveDomain[_] = null;
  //
  //  @Before
  //  def setUp() {
  //    intDomain = IntDomain(Seq(1, 7, 9));
  //    //anyDomain = ExtensiveDomain.of(8.9d, 1, intDomain);
  //  }
  //
  //  @Test
  //  def testEquals() {
  //    assertEquals(Set(1, 7, 9), intDomain);
  //  }
  //
  //  @Test
  //  def testToString() {
  //    assertEquals("{1, 7, 9}", intDomain.toString);
  //    //assertEquals(anyDomain.toString, "{8.9, 1, {1, 7, 9}}");
  //  }

  "Extensive domains" should "behave like sets" in {
    forAll { s: SortedSet[Int] =>

      whenever(s.nonEmpty && s.size != (s.max - s.min + 1)) {

        val intDomain = IntDomain(s.toSeq)

        s shouldBe intDomain
        intDomain shouldBe s

      }

    }
  }

}
